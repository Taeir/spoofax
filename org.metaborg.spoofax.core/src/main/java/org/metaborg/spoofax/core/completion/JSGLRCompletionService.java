package org.metaborg.spoofax.core.completion;

import java.util.*;
import java.util.stream.Collectors;

import javax.annotation.Nullable;

import mb.nabl2.constraints.Constraints;
import mb.nabl2.constraints.IConstraint;
import mb.nabl2.constraints.ast.AstProperties;
import mb.nabl2.scopegraph.terms.Occurrence;
import mb.nabl2.scopegraph.terms.Scope;
import mb.nabl2.solver.ISolution;
import mb.nabl2.solver.SolverException;
import mb.nabl2.solver.messages.Messages;
import mb.nabl2.solver.solvers.CallExternal;
import mb.nabl2.solver.solvers.CompletionSolver;
import mb.nabl2.spoofax.analysis.IResult;
import mb.nabl2.stratego.*;
import mb.nabl2.terms.ITerm;
import mb.nabl2.terms.matching.Transform;
import mb.nabl2.terms.unification.IUnifier;
import org.apache.commons.vfs2.FileObject;
import org.metaborg.core.MetaborgException;
import org.metaborg.core.completion.Completion;
import org.metaborg.core.completion.CompletionKind;
import org.metaborg.core.completion.ICompletion;
import org.metaborg.core.language.ILanguageComponent;
import org.metaborg.core.language.ILanguageImpl;
import org.metaborg.core.resource.IResourceService;
import org.metaborg.core.source.ISourceLocation;
import org.metaborg.core.source.ISourceRegion;
import org.metaborg.core.source.SourceLocation;
import org.metaborg.core.source.SourceRegion;
import org.metaborg.core.syntax.ParseException;
import org.metaborg.spoofax.core.context.constraint.IConstraintContext;
import org.metaborg.spoofax.core.stratego.IStrategoCommon;
import org.metaborg.spoofax.core.stratego.IStrategoRuntimeService;
import org.metaborg.spoofax.core.syntax.ISpoofaxSyntaxService;
import org.metaborg.spoofax.core.syntax.JSGLRParserConfiguration;
import org.metaborg.spoofax.core.syntax.JSGLRSourceRegionFactory;
import org.metaborg.spoofax.core.syntax.SourceAttachment;
import org.metaborg.spoofax.core.syntax.SyntaxFacet;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.unit.*;
import org.metaborg.util.functions.Function1;
import org.metaborg.util.iterators.Iterables2;
import org.metaborg.util.task.NullCancel;
import org.metaborg.util.task.NullProgress;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spoofax.interpreter.core.Tools;
import org.spoofax.interpreter.terms.*;
import org.spoofax.jsglr.client.imploder.IToken;
import org.spoofax.jsglr.client.imploder.ITokens;
import org.spoofax.jsglr.client.imploder.ImploderAttachment;
import org.spoofax.jsglr.client.imploder.ListImploderAttachment;
import org.spoofax.terms.AbstractTermFactory;
import org.spoofax.terms.StrategoAppl;
import org.spoofax.terms.StrategoConstructor;
import org.spoofax.terms.StrategoTerm;
import org.spoofax.terms.attachments.OriginAttachment;
import org.spoofax.terms.attachments.ParentAttachment;
import org.spoofax.terms.visitor.AStrategoTermVisitor;
import org.spoofax.terms.visitor.IStrategoTermVisitor;
import org.spoofax.terms.visitor.StrategoTermVisitee;
import org.strategoxt.HybridInterpreter;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.inject.Inject;

public class JSGLRCompletionService implements ISpoofaxCompletionService {
    private static final Logger logger = LoggerFactory.getLogger(JSGLRCompletionService.class);

    private static final String PLACEHOLDER_SORT_SUFFIX = "-Plhdr";

    private final ITermFactoryService termFactoryService;
    private final IStrategoRuntimeService strategoRuntimeService;
    private final IStrategoCommon strategoCommon;
    private final IResourceService resourceService;
    private final ISpoofaxUnitService unitService;
    private final ISpoofaxSyntaxService syntaxService;



    @Inject public JSGLRCompletionService(ITermFactoryService termFactoryService,
        IStrategoRuntimeService strategoRuntimeService, IStrategoCommon strategoCommon,
        IResourceService resourceService, ISpoofaxUnitService unitService, ISpoofaxSyntaxService syntaxService) {
        this.termFactoryService = termFactoryService;
        this.strategoRuntimeService = strategoRuntimeService;
        this.strategoCommon = strategoCommon;
        this.resourceService = resourceService;
        this.unitService = unitService;
        this.syntaxService = syntaxService;
    }

    @Override public Iterable<ICompletion> get(int position, ISpoofaxParseUnit parseInput, ISpoofaxAnalyzeUnit analyzeInput, boolean nested)
        throws MetaborgException {

        List<ICompletion> completions = new ArrayList<>();

        // Completion in case of empty input
        String inputText = parseInput.input().text();
        if(inputText.trim().isEmpty()) {
            final ILanguageImpl language = parseInput.input().langImpl();
            final FileObject location = parseInput.source();
            final Iterable<String> startSymbols = language.facet(SyntaxFacet.class).startSymbols;
            completions.addAll(completionEmptyProgram(startSymbols, inputText.length(), language, location));
            
            return completions;
        }
        
        ISpoofaxParseUnit completionParseResult = null;
        if(!parseInput.success() && !nested) {
        	// When parsing failed, and we are not in a nested scope,
        	// reparse, with recovery and completions enabled.
            final JSGLRParserConfiguration config = new JSGLRParserConfiguration(true, true, true, 3000, position);
            final ISpoofaxInputUnit input = parseInput.input();
            final ISpoofaxInputUnit modifiedInput =
                unitService.inputUnit(input.source(), input.text(), input.langImpl(), input.dialect(), config);
            completionParseResult = syntaxService.parse(modifiedInput);
        }

        if(completionParseResult != null && completionParseResult.ast() == null) {
            return completions;
        }
        
        Collection<IStrategoTerm> nestedCompletionTerms = getNestedCompletionTermsFromAST(completionParseResult);
        if(!nestedCompletionTerms.isEmpty()) {
            completions.addAll(completionErroneousProgramsNested(position, nestedCompletionTerms, completionParseResult));
        }
        
        Collection<IStrategoTerm> completionTerms = getCompletionTermsFromAST(completionParseResult);
        if(!completionTerms.isEmpty()) {
            completions.addAll(completionErroneousPrograms(position, completionTerms, completionParseResult));
        }

        if(completionTerms.isEmpty() && nestedCompletionTerms.isEmpty()) {
        	boolean blankLineCompletion = isCompletionBlankLine(position, parseInput.input().text());
            completions.addAll(completionCorrectPrograms(position, blankLineCompletion, parseInput, analyzeInput));
        }

        // Sort the completions by name.
        completions.sort(Comparator.comparing(ICompletion::name));
        return completions;

    }

    public Collection<? extends ICompletion> completionEmptyProgram(Iterable<String> startSymbols, int endOffset,
        ILanguageImpl language, FileObject location) throws MetaborgException {
        Collection<ICompletion> completions = Lists.newLinkedList();

        final String languageName = language.belongsTo().name();

        for(ILanguageComponent component : language.components()) {
            // call Stratego part of the framework to compute change
            final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
            final ITermFactory termFactory = termFactoryService.get(component, null, false);

            for(String startSymbol : startSymbols) {
                String placeholderName = startSymbol + PLACEHOLDER_SORT_SUFFIX;
                IStrategoAppl placeholder = termFactory.makeAppl(termFactory.makeConstructor(placeholderName, 0));
                IStrategoTuple input = termFactory.makeTuple(termFactory.makeString(startSymbol), placeholder);

                final IStrategoTerm proposalsPlaceholder =
                    strategoCommon.invoke(runtime, input, "get-proposals-empty-program-" + languageName);

                if(proposalsPlaceholder == null) {
                    logger.error("Getting proposals for {} failed", placeholder);
                    continue;
                }

                for(IStrategoTerm proposalTerm : proposalsPlaceholder) {
                    if(!(proposalTerm instanceof IStrategoTuple)) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                    if(tuple.getSubtermCount() != 2 || !(tuple.getSubterm(0) instanceof IStrategoString)
                        || !(tuple.getSubterm(1) instanceof IStrategoString)) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    final String name = Tools.asJavaString(tuple.getSubterm(0));
                    final String text = Tools.asJavaString(tuple.getSubterm(1));
                    final String additionalInfo = Tools.asJavaString(tuple.getSubterm(1));

                    completions.add(new Completion(name, startSymbol, text, additionalInfo, 0, endOffset,
                        CompletionKind.expansion));
                }
            }
        }

        return completions;
    }



    private boolean isCompletionBlankLine(int position, String text) {
        int i = position - 1;
        while(i >= 0) {
            if(text.charAt(i) == '\n') {
                break;
            } else if(text.charAt(i) == ' ' || text.charAt(i) == '\t') {
                i--;
                continue;
            } else
                return false;
        }
        i = position;
        while(i < text.length()) {
            if(text.charAt(i) == '\n') {
                break;
            } else if(text.charAt(i) == ' ' || text.charAt(i) == '\t') {
                i++;
                continue;
            } else
                return false;
        }

        return true;
    }

    public Collection<ICompletion> completionCorrectPrograms(
            int position,
            boolean blankLineCompletion,
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult) throws MetaborgException {
        if (analysisResult == null && parseResult == null)
            return Collections.emptyList();

        final Collection<ICompletion> completions = Sets.newHashSet();
        final FileObject location = parseResult.source();
        final ISpoofaxInputUnit input = parseResult.input();
        final ILanguageImpl language = input.langImpl();
        final String languageName = language.belongsTo().name();
//        final IStrategoTerm ast = parseResult.ast();
        final IStrategoTerm ast = analysisResult != null ? analysisResult.ast() : parseResult.ast();

        for(ILanguageComponent component : language.components()) {


            final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
            final ITermFactory termFactory = termFactoryService.get(component, null, false);

            final Map<IStrategoTerm, Boolean> leftRecursiveTerms = new HashMap<IStrategoTerm, Boolean>();
            final Map<IStrategoTerm, Boolean> rightRecursiveTerms = new HashMap<IStrategoTerm, Boolean>();

            List<IStrategoTerm> terms = findAstTermsAtOffset(ast, position, runtime, termFactory, leftRecursiveTerms, rightRecursiveTerms);
            @Nullable IStrategoAppl placeholder = getPlaceholderFromTerms(terms, position);
//            final Iterable<IStrategoTerm> terms =
//                tracingTermsCompletions(position, ast, new SourceRegion(position), runtime, termFactory,
//                    languageName, leftRecursiveTerms, rightRecursiveTerms);

//            final IStrategoAppl placeholder = getPlaceholder(position, terms);
            final Iterable<IStrategoList> lists = getLists(terms, leftRecursiveTerms, rightRecursiveTerms);
            final Iterable<IStrategoTerm> optionals = getOptionals(terms, leftRecursiveTerms, rightRecursiveTerms);
            final Iterable<IStrategoTerm> leftRecursive = getLeftRecursiveTerms(position, terms, leftRecursiveTerms);
            final Iterable<IStrategoTerm> rightRecursive = getRightRecursiveTerms(position, terms, rightRecursiveTerms);

            if(placeholder != null) {
                @Nullable IStrategoTerm placeholderParent = getParentTermOf(placeholder, terms);
                completions.addAll(placeholderCompletions(placeholder, placeholderParent, languageName, component, language, location, analysisResult));
            } else {
                if(Iterables.size(lists) != 0) {
                    completions.addAll(
                        listsCompletions(position, blankLineCompletion, lists, languageName, component, location));
                }

                if(Iterables.size(optionals) != 0) {
                    completions
                        .addAll(optionalCompletions(optionals, blankLineCompletion, languageName, component, location));
                }
                // TODO Improve recursive completions
                // if(Iterables.size(leftRecursive) != 0 || Iterables.size(rightRecursive) != 0) {
                // completions
                // .addAll(recursiveCompletions(leftRecursive, rightRecursive, languageName, component, location));
                // }
            }
        }

        return completions;
    }

    /**
     *
     * @param placeholder
     * @param syntacticProposals A list of proposals in the form (name, text, additionalInfo, REPLACE_TERM(oldNode, newNode)).
     * @param context
     * @param termFactory
     * @return
     */
    private Collection<ICompletion> semanticPlaceholderCompletions(IStrategoAppl placeholder, IStrategoTerm syntacticProposals, IConstraintContext context, ITermFactory termFactory, HybridInterpreter runtime, ILanguageImpl language) {
        Collection<ICompletion> proposals = new ArrayList<>();

        // From the placeholder, the most specific element:
        logger.debug("Semantic completing on placeholder: " + placeholder.toString());

        StrategoTerms strategoTerms = new StrategoTerms(termFactory);
        CompletionSolver solver = new CompletionSolver(CallExternal.never());

        @Nullable TermIndex termIndex = getTermIndex(placeholder);
        if (termIndex == null) return Collections.emptyList();
        @Nullable IResult result = getAnalysisResult(termIndex, context);
        if (result == null) return Collections.emptyList();

        final SolutionHelper solutionHelper = new SolutionHelper(result.solution());

        Function1<String, String> fresh = result.fresh().melt()::fresh;

        // Get the term's [[ _ ^ params : type ]].
        @Nullable ITerm params = solutionHelper.getParamsOfTerm(termIndex);// result.solution().astProperties().getValue(termIndex, AstProperties.PARAMS_KEY).orElse(null);
        @Nullable ITerm type = solutionHelper.getTypeOfTerm(termIndex);// result.solution().astProperties().getValue(termIndex, AstProperties.TYPE_KEY).orElse(null);
        if (params == null) return Collections.emptyList();

        // Get the scopes from the args.
        Collection<Scope> scopes = solutionHelper.getScopesOfTerm(termIndex);
//        IUnifier unifier = result.solution().unifier();
//        Collection<Scope> scopes = Transform.T.collecttd(t -> Scope.matcher().match(t, unifier)).apply(params);

        Set<Occurrence> occurrences = getOccurrencesInScopes(scopes, solutionHelper);
        for (IStrategoTerm syntaxProposal : syntacticProposals) {

            if (!(syntaxProposal instanceof IStrategoTuple)) {
                logger.error("Unexpected proposal term {}, skipping", syntaxProposal);
                continue;
            }
            final IStrategoTuple tuple = (IStrategoTuple)syntaxProposal;
            if (tuple.getSubtermCount() != 4 || !(tuple.getSubterm(0) instanceof IStrategoString)
                    || !(tuple.getSubterm(1) instanceof IStrategoString)
                    || !(tuple.getSubterm(2) instanceof IStrategoString)
                    || !(tuple.getSubterm(3) instanceof IStrategoAppl)) {
                logger.error("Unexpected proposal term {}, skipping", syntaxProposal);
                continue;
            }

            final String name = Tools.asJavaString(tuple.getSubterm(0));
            final String text = Tools.asJavaString(tuple.getSubterm(1));
            final String additionalInfo = Tools.asJavaString(tuple.getSubterm(2));
            final StrategoAppl change = (StrategoAppl)tuple.getSubterm(3);

            if (change.getConstructor().getName().contains("REPLACE_TERM")) {
                IStrategoTerm syntaxFragment = change.getSubterm(1);
                boolean isValid = isSemanticallyValidFragment(syntaxFragment, params, type, result, fresh, strategoTerms, solver, termFactory, runtime);
                if (isValid) {
                    // Build a completion from it.
                    final ICompletion proposal = createCompletionReplaceTerm("+ "+ name, text, additionalInfo, change, false, "", "");
                    proposals.add(proposal);
                }

//                List<ICompletion> fragmentProposals = findSemanticCompletionsForFragment(name, text, additionalInfo, change, syntaxFragment, params, type, fresh, result, occurrences, strategoTerms, solver, termFactory, runtime, language);
//                proposals.addAll(fragmentProposals);
            }
        }

        return proposals;
    }


    /**
     * Returns the term index for the specified Stratego term.
     *
     * @param term The Stratego term.
     * @return The term index.
     */
    @Nullable private TermIndex getTermIndex(IStrategoTerm term) {
        return StrategoTermIndices.get(term).orElse(null);
    }

    /**
     * Gets the analysis result from the specified term.
     *
     * @param termIndex The term index.
     * @param context The constraint context.
     * @return The analysis result.
     */
    @Nullable private IResult getAnalysisResult(TermIndex termIndex, IConstraintContext context) {
        String resource = termIndex.getResource();
        IStrategoTerm analysisTerm = context.getAnalysis(resource);
        return StrategoBlob.match(analysisTerm, IResult.class).orElse(null);
    }

    /**
     * Finds the valid semantic completions of the specified syntactic fragment.
     *
     * @param syntaxFragment The syntax fragment.
     * @param params The AST term 'params' property.
     * @param type The AST term 'type' property.
     * @param fresh The fresh name function.
     * @param result The analysis result.
     * @param occurrences The occurrences.
     * @param strategoTerms The Stratego-Nabl term helper.
     * @param solver The solver.
     * @param factory The term factory.
     * @return The completion proposals.
     */
    private List<ICompletion> findSemanticCompletionsForFragment(String name, String text, String additionalInfo, StrategoAppl change, IStrategoTerm syntaxFragment, ITerm params, @Nullable ITerm type, Function1<String, String> fresh, IResult result, Set<Occurrence> occurrences, StrategoTerms strategoTerms, CompletionSolver solver, ITermFactory factory, HybridInterpreter runtime, ILanguageImpl language) {
        ArrayList<ICompletion> proposals = Lists.newArrayList();
        for (Occurrence occurrence : occurrences) {
            // Get the name of the occurrence.
            ITerm nablOccurrenceName = occurrence.getName();
            IStrategoTerm strOccurrenceName = strategoTerms.toStratego(nablOccurrenceName);
            String occurrenceName = ((IStrategoString)strOccurrenceName).stringValue();

            // Get the subterm index
            int placeholderIndex = getFirstPlaceholderIndexInTerm(syntaxFragment);
            if (placeholderIndex == -1) continue;
            IStrategoAppl placeholder = (IStrategoAppl)syntaxFragment.getSubterm(placeholderIndex);

            // Build the fragment.
            IStrategoTerm fragment = buildFragment(syntaxFragment, occurrenceName, placeholderIndex, factory);
//            IStrategoTerm fragment = buildFragment2(text, placeholder, occurrenceName, language);
            if (fragment == null) continue;

            boolean isValid = isSemanticallyValidFragment(fragment, params, type, result, fresh, strategoTerms, solver, factory, runtime);

            if (isValid) {
                // Build a completion from it.
                ICompletion proposal = buildCompletionProposal(name, text, additionalInfo, change, fragment, placeholder, occurrenceName);
                proposals.add(proposal);
            }
        }

        return proposals;
    }

    /**
     * Gets the zero-based index of the subterm (the first placeholder) that will be replaced.
     *
     * @param term The term.
     * @return The zero-based index of the subterm; or -1 when no placeholder was found.
     */
    private int getFirstPlaceholderIndexInTerm(IStrategoTerm term) {
        // TODO: Return only the first placeholder that:
        // 1. Is of a lexical sort (e.g. lexical syntax: ID); or
        // 2. Is directly rewritable to a lexical sort (e.g. context-free syntax: Occ = ID).
        // Also, the placeholder should be able to parse the name validly. For example, `ab2cd` is not a valid ID
        // when IDs cannot contain digits, so we must not try it.
        // In other words: we want to prevent us from producing a fragment of an invalid AST.
        // Alternatively, we create the string and parse it as an AST fragment, but this has a big overhead.

        IStrategoTerm[] subterms = term.getAllSubterms();
        for (int i = 0; i < subterms.length; i++) {
            if (isPlaceholderTerm(subterms[i])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Builds a fragment from the specified syntax fragment and occurrence.
     *
     * @param fragment The syntax fragment, for example {@literal FieldVar(LValue-Plhdr,Occ-Plhdr)}.
     * @param occurrence The occurrence.
     * @param placeholderIndex The placeholder index.
     * @param factory The term factory.
     * @return The built fragment with the name of the occurrence integrated into it; or null.
     */
    @Nullable private IStrategoTerm buildFragment(IStrategoTerm fragment, String occurrence, int placeholderIndex, ITermFactory factory) {
        // The fragment is not a term application.
        if (!(fragment instanceof IStrategoAppl)) return null;
        IStrategoAppl fragmentAppl = (IStrategoAppl)fragment;

        // Replace the first placeholder by the name of the occurrence.
        IStrategoTerm[] subterms = fragmentAppl.getAllSubterms().clone();
        subterms[placeholderIndex] = factory.makeString(occurrence);

        return factory.replaceAppl(fragmentAppl.getConstructor(), subterms, fragmentAppl);
    }

    @Nullable private IStrategoTerm buildFragment2(String originalFragment, IStrategoAppl placeholder, String occurrenceName, ILanguageImpl language) {
        String sort = placeholder.getConstructor().getName();
        sort = sort.substring(0, sort.length() - PLACEHOLDER_SORT_SUFFIX.length());
        String placeholderName = "$" + sort;

        String newFragment = replaceOnce(originalFragment, placeholderName, occurrenceName);
        return tryParseFragment(newFragment, sort, language);
    }

    /**
     * Attempts to parse the given fragment.
     *
     * @param fragment The fragment.
     * @param sort The sort of the fragment (i.e. the parser start symbol).
     * @param language The language of the fragment.
     * @return The fragment AST; or null when parsing failed.
     */
    @Nullable private IStrategoTerm tryParseFragment(String fragment, String sort, ILanguageImpl language) {
        int timeout = 0;//JSGLRParserConfiguration.defaultTimeout;
        final JSGLRParserConfiguration config = new JSGLRParserConfiguration(true, false, false,
                timeout, JSGLRParserConfiguration.defaultCursorPosition, sort);
        final ISpoofaxInputUnit input = unitService.inputUnit(fragment, language, null, config);
        final ISpoofaxParseUnit parseResult;
        try {
            parseResult = syntaxService.parse(input);
        } catch (ParseException e) {
            logger.error("Unexpected parse exception while finding completions.", e);
            return null;
        }

        if (!parseResult.success()) return null;

        return parseResult.ast();
    }

    /**
     * Determines whether the specified fragment is semantically valid.
     *
     * @param fragment The fragment to test.
     * @param result The original analysis result.
     * @param fresh Function for fresh names.
     * @return True when the fragment is semantically valid;
     * otherwise, false.
     */
    private boolean isSemanticallyValidFragment(IStrategoTerm fragment, ITerm params, @Nullable ITerm type, IResult result, Function1<String, String> fresh, StrategoTerms strategoTerms, CompletionSolver solver, ITermFactory factory, HybridInterpreter runtime) {



        // Create the constraint [[ fragment ^ params : type ]] or [[ fragment ^ params ]] (when there is NoType).
        // args = Params(params) (in case of NoType()) or ParamsAndType(params, type)


        IStrategoTerm paramsStr = strategoTerms.toStratego(ConstraintTerms.explicate(params));
        // constraintTerm = <nabl2--generate-constraint-completion> (strategoName, fragment, args)
        IStrategoTerm args;
        if (type != null) {
            IStrategoTerm typeStr = strategoTerms.toStratego(ConstraintTerms.explicate(type));
            IStrategoConstructor params2cons = factory.makeConstructor("ParamsAndType", 2);
            args = factory.makeAppl(params2cons, paramsStr, typeStr);
        } else {
            IStrategoConstructor params1cons = factory.makeConstructor("Params", 1);
            args = factory.makeAppl(params1cons, paramsStr);
        }

        IStrategoTerm indexedFragment = StrategoTermIndices.index(fragment, "tigertest.tig#completion", factory);
        IStrategoTerm input = factory.makeTuple(factory.makeString("tigertest.tig"), indexedFragment, args);
        IStrategoTerm constraintTerm = null;
        try {
            // The strategy should not fail (i.e. return null or CFalse()).
            constraintTerm = this.strategoCommon.invoke(runtime, input, "nabl2--generate-constraint-completion");
        } catch(MetaborgException e) {
            logger.error("Could not generate constraint completion for {}", input);
        }

        if (constraintTerm == null) {
            logger.warn("Constraint completion failed for {}", input);
            return false;
        }

        ITerm constraintStrategoTerm = strategoTerms.fromStratego(constraintTerm);
        @Nullable IConstraint constraint = ConstraintTerms.specialize(Constraints.matcher()).match(constraintStrategoTerm).orElse(null);
        if (constraint == null) return false;
        List<IConstraint> completionConstraints = Collections.singletonList(constraint);

        // Create a new solution with the new constraints added to it.
        // We remove the messages from the solution so that we can see any errors resulting from adding the constraint.
        HashSet<IConstraint> newConstraints = new HashSet<>(result.solution().constraints());
        newConstraints.addAll(completionConstraints);
        ISolution oldSolution = result.solution()
                .withConstraints(newConstraints)
                .withMessages(Messages.Immutable.of());
        ISolution newSolution = null;
        try {
            newSolution = solver.solve(oldSolution, fresh, new NullCancel(), new NullProgress());
        } catch (SolverException | InterruptedException e) {
            throw new RuntimeException(e);
        }
        
        // If the solution does not contain any new errors,
        // the fragment is semantically and syntactically valid and can be suggested as a completion
        return newSolution.messages().getErrors().isEmpty();

        // NOTE: This is not sound when using a scope graph across multiple files:
        //
        //      I
        //     / \
        //    U1 U2
        //     \ /
        //      F
        //
        // When a constraint is added to U2 (unit 2) in this situation,
        // then F is not recomputed with the new constraint. It is possible that the new constraint
        // changes the final whole-program analysis.
    }
    
    private Object pp(ISolution newSolution) {
    	return newSolution.constraints().stream().map(c -> c.pp().toString(newSolution.unifier()::toString)).collect(Collectors.toList());
    }

    /**
     * Builds a completion proposal from the specified fragment.
     *
     * @return The completion proposal.
     */
    private ICompletion buildCompletionProposal(String name, String text, String additionalInfo, StrategoAppl change, IStrategoTerm newFragment, IStrategoAppl placeholder, @Nullable String occurrenceName) {

        String placeholderName = "$" + placeholder.getConstructor().getName();
        placeholderName = placeholderName.substring(0, placeholderName.length() - PLACEHOLDER_SORT_SUFFIX.length());

        if (occurrenceName != null) {
            text = replaceOnce(text, placeholderName, occurrenceName);
            additionalInfo = replaceOnce(additionalInfo, placeholderName, occurrenceName);
        }

        return createCompletionReplaceTerm(occurrenceName + " (" + name + ")", text, additionalInfo, change, false, "", "");
    }

    private String replaceOnce(String text, String from, String to) {
        int i = text.indexOf(from);
        if (i < 0) return text;
        return text.substring(0, i) + to + text.substring(i + from.length());
    }

    /**
     * Gets the occurrences in the specified scopes.
     *
     * @param scopes The scopes to look into.
     * @param solutionHelper The solution helper to query.
     * @return A set of occurrences.
     */
    private Set<Occurrence> getOccurrencesInScopes(Iterable<Scope> scopes, SolutionHelper solutionHelper) {
        HashSet<Occurrence> occurrences = Sets.newHashSet();
        for (Scope scope : scopes) {
            occurrences.addAll(solutionHelper.getOccurrencesInScope(scope));
        }
        return occurrences;
    }

    public Collection<ICompletion> placeholderCompletions(IStrategoAppl placeholder, @Nullable IStrategoTerm placeholderParent, String languageName,
        ILanguageComponent component, ILanguageImpl language, FileObject location, @Nullable ISpoofaxAnalyzeUnit analysisResult) throws MetaborgException {
        Collection<ICompletion> completions = Lists.newLinkedList();


        // call Stratego part of the framework to compute change
        final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
        final ITermFactory termFactory = termFactoryService.get(component, null, false);

        final IStrategoTerm proposalsPlaceholder = strGetPlaceholderCompletions(placeholder, placeholderParent, languageName, runtime, termFactory);

        if(proposalsPlaceholder != null) {

            // Syntactic completions
            for (IStrategoTerm proposalTerm : proposalsPlaceholder) {
                if (!(proposalTerm instanceof IStrategoTuple)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }
                final IStrategoTuple tuple = (IStrategoTuple)proposalTerm;
                if (tuple.getSubtermCount() != 4 || !(tuple.getSubterm(0) instanceof IStrategoString)
                        || !(tuple.getSubterm(1) instanceof IStrategoString)
                        || !(tuple.getSubterm(2) instanceof IStrategoString)
                        || !(tuple.getSubterm(3) instanceof IStrategoAppl)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }

                final String name = Tools.asJavaString(tuple.getSubterm(0));
                final String text = Tools.asJavaString(tuple.getSubterm(1));
                final String additionalInfo = Tools.asJavaString(tuple.getSubterm(2));
                final StrategoAppl change = (StrategoAppl)tuple.getSubterm(3);

                if (change.getConstructor().getName().contains("REPLACE_TERM")) {
                    final ICompletion completion =
                            createCompletionReplaceTerm(name, text, additionalInfo, change, false, "", "");

                    if (completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                }
            }

            // Semantic completions
            if (analysisResult != null && analysisResult.context() instanceof IConstraintContext) {
                completions.addAll(semanticPlaceholderCompletions(placeholder, proposalsPlaceholder, (IConstraintContext)analysisResult.context(), termFactory, runtime, language));
            }
        }

        return completions;
    }

    private IStrategoTerm strGetPlaceholderCompletions(
            IStrategoAppl placeholder,
            @Nullable IStrategoTerm placeholderParent,
            String languageName,
            HybridInterpreter runtime,
            ITermFactory termFactory)
            throws MetaborgException {

//        placeholderParent = placeholderParent != null ? placeholderParent : ParentAttachment.getParent(placeholder);
        placeholderParent = ParentAttachment.getParent(placeholder);
        if (placeholderParent == null) {
            placeholderParent = placeholder;
        }

        final IStrategoAppl placeholderWithoutAnnos = stripAnnos(placeholder, termFactory);

        final IStrategoInt placeholderIdx = termFactory.makeInt(getTermIndexInParent(placeholderWithoutAnnos, placeholderParent));

        final String sort = ImploderAttachment.getSort(placeholder);
        final IStrategoTerm strategoInput =
                termFactory.makeTuple(termFactory.makeString(sort), placeholderWithoutAnnos, placeholderParent, placeholderIdx);

        final IStrategoTerm proposalsPlaceholder =
                strategoCommon.invoke(runtime, strategoInput, "get-proposals-placeholder-" + languageName);
        // [
        // ("Seq","(\n          ##CURSOR##\n        )","(\n  ##CURSOR##\n)",REPLACE_TERM(Exp-Plhdr,Seq(CURSOR_ELEMENT))),
        // ("If","if $Exp then\n          $Exp\n        else\n          $Exp","if $Exp then\n  $Exp\nelse\n  $Exp",REPLACE_TERM(Exp-Plhdr,If(Exp-Plhdr,Exp-Plhdr,Exp-Plhdr))),
        // ("IfThen","if $Exp then\n          $Exp","if $Exp then\n  $Exp",REPLACE_TERM(Exp-Plhdr,IfThen(Exp-Plhdr,Exp-Plhdr))),
        // ("While","while $Exp do\n          $Exp","while $Exp do\n  $Exp",REPLACE_TERM(Exp-Plhdr,While(Exp-Plhdr,Exp-Plhdr))),
        // ("For","for $Var := $Exp to $Exp do\n          $Exp","for $Var := $Exp to $Exp do\n  $Exp",REPLACE_TERM(Exp-Plhdr,For(Var-Plhdr,Exp-Plhdr,Exp-Plhdr,Exp-Plhdr))),
        // ("Break","break","break",REPLACE_TERM(Exp-Plhdr,Break)),
        // ("Array","$TypeId[$Exp] of $Exp","$TypeId[$Exp] of $Exp",REPLACE_TERM(Exp-Plhdr,Array(TypeId-Plhdr,Exp-Plhdr,Exp-Plhdr))),
        // ("NilExp","nil","nil",REPLACE_TERM(Exp-Plhdr,NilExp)),
        // ("Record","$TypeId{ ##CURSOR## }","$TypeId{ ##CURSOR## }",REPLACE_TERM(Exp-Plhdr,Record(TypeId-Plhdr,CURSOR_ELEMENT))),
        // ("String","$StrConst","$StrConst",REPLACE_TERM(Exp-Plhdr,String(StrConst-Plhdr))),
        // ("Int","$IntConst","$IntConst",REPLACE_TERM(Exp-Plhdr,Int(IntConst-Plhdr))),
        // ("Uminus","- $Exp","- $Exp",REPLACE_TERM(Exp-Plhdr,Uminus(Exp-Plhdr))),
        // ("Times","$Exp * $Exp","$Exp * $Exp",REPLACE_TERM(Exp-Plhdr,Times(Exp-Plhdr,Exp-Plhdr))),
        // ("Divide","$Exp / $Exp","$Exp / $Exp",REPLACE_TERM(Exp-Plhdr,Divide(Exp-Plhdr,Exp-Plhdr))),
        // ("Plus","$Exp + $Exp","$Exp + $Exp",REPLACE_TERM(Exp-Plhdr,Plus(Exp-Plhdr,Exp-Plhdr))),
        // ("Minus","$Exp - $Exp","$Exp - $Exp",REPLACE_TERM(Exp-Plhdr,Minus(Exp-Plhdr,Exp-Plhdr))),
        // ("Eq","$Exp = $Exp","$Exp = $Exp",REPLACE_TERM(Exp-Plhdr,Eq(Exp-Plhdr,Exp-Plhdr))),
        // ("Neq","$Exp <> $Exp","$Exp <> $Exp",REPLACE_TERM(Exp-Plhdr,Neq(Exp-Plhdr,Exp-Plhdr))),
        // ("Gt","$Exp > $Exp","$Exp > $Exp",REPLACE_TERM(Exp-Plhdr,Gt(Exp-Plhdr,Exp-Plhdr))),
        // ("Lt","$Exp < $Exp","$Exp < $Exp",REPLACE_TERM(Exp-Plhdr,Lt(Exp-Plhdr,Exp-Plhdr))),
        // ("Geq","$Exp >= $Exp","$Exp >= $Exp",REPLACE_TERM(Exp-Plhdr,Geq(Exp-Plhdr,Exp-Plhdr))),
        // ("Leq","$Exp <= $Exp","$Exp <= $Exp",REPLACE_TERM(Exp-Plhdr,Leq(Exp-Plhdr,Exp-Plhdr))),
        // ("And","$Exp & $Exp","$Exp & $Exp",REPLACE_TERM(Exp-Plhdr,And(Exp-Plhdr,Exp-Plhdr))),
        // ("Or","$Exp | $Exp","$Exp | $Exp",REPLACE_TERM(Exp-Plhdr,Or(Exp-Plhdr,Exp-Plhdr))),
        // ("Call","$Occ(##CURSOR##)","$Occ(##CURSOR##)",REPLACE_TERM(Exp-Plhdr,Call(Occ-Plhdr,CURSOR_ELEMENT))),
        // ("Subscript","$LValue[$Index]","$LValue[$Index]",REPLACE_TERM(Exp-Plhdr,Subscript(LValue-Plhdr,Index-Plhdr))),
        // ("FieldVar","$LValue.$Occ","$LValue.$Occ",REPLACE_TERM(Exp-Plhdr,FieldVar(LValue-Plhdr,Occ-Plhdr))),
        // ("Var","$Occ","$Occ",REPLACE_TERM(Exp-Plhdr,Var(Occ-Plhdr))),
        // ("Assign","$LValue := $Exp","$LValue := $Exp",REPLACE_TERM(Exp-Plhdr,Assign(LValue-Plhdr,Exp-Plhdr))),
        // ("Let","let\n          ##CURSOR##\n         in\n          ##CURSOR##\n        end","let\n  ##CURSOR##\n in\n  ##CURSOR##\nend",REPLACE_TERM(Exp-Plhdr,Let(CURSOR_ELEMENT,CURSOR_ELEMENT)))
        // ]

        if (proposalsPlaceholder == null) {
            logger.error("Getting proposals for {} failed", placeholder);
        }
        return proposalsPlaceholder;
    }

    /**
     * Strips all annotations of the specified term.
     *
     * @param term The term to strip.
     * @param termFactory The term factory.
     * @param <T> The type of term.
     * @return The term without its annotations.
     */
    private <T extends IStrategoTerm> T stripAnnos(T term, ITermFactory termFactory) {
        //noinspection deprecation,unchecked
        return (T)termFactory.annotateTerm(term, AbstractTermFactory.EMPTY_LIST);
    }

    /**
     * Gets the zero-based index of the term in the parent term.
     *
     * @param term The term to find.
     * @param parent The parent term.
     * @return The zero-based index of the term; or -1 when not found.
     */
    private int getTermIndexInParent(IStrategoAppl term, IStrategoTerm parent) {
        for (int i = 0; i < parent.getSubtermCount(); i++) {
            if (parent.getSubterm(i).match(term)) {
                return i;
            }
        }
        return -1;
    }

    public Collection<ICompletion> optionalCompletions(Iterable<IStrategoTerm> optionals, boolean blankLineCompletion,
        String languageName, ILanguageComponent component, FileObject location) throws MetaborgException {

        Collection<ICompletion> completions = Lists.newLinkedList();

        final ITermFactory termFactory = termFactoryService.get(component, null, false);

        for(IStrategoTerm optional : optionals) {

            ImploderAttachment attachment = optional.getAttachment(ImploderAttachment.TYPE);
            String sort = attachment.getSort();
            String placeholderName = sort + "-Plhdr";
            IStrategoAppl optionalPlaceholder = termFactory.makeAppl(termFactory.makeConstructor(placeholderName, 0));
            final IStrategoTerm strategoInput =
                termFactory.makeTuple(termFactory.makeString(sort), optional, optionalPlaceholder);

            // call Stratego part of the framework to compute change
            final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
            final IStrategoTerm proposalsOptional =
                strategoCommon.invoke(runtime, strategoInput, "get-proposals-optional-" + languageName);

            if(proposalsOptional == null) {
                logger.error("Getting proposals for {} failed", strategoInput);
                continue;
            }

            for(IStrategoTerm proposalTerm : proposalsOptional) {
                if(!(proposalTerm instanceof IStrategoTuple)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }
                final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                if(tuple.getSubtermCount() != 4 || !(tuple.getSubterm(0) instanceof IStrategoString)
                    || !(tuple.getSubterm(1) instanceof IStrategoString)
                    || !(tuple.getSubterm(2) instanceof IStrategoString)
                    || !(tuple.getSubterm(3) instanceof IStrategoAppl)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }

                final String name = Tools.asJavaString(tuple.getSubterm(0));
                final String text = Tools.asJavaString(tuple.getSubterm(1));
                final String additionalInfo = Tools.asJavaString(tuple.getSubterm(2));
                final StrategoAppl change = (StrategoAppl) tuple.getSubterm(3);

                if(change.getConstructor().getName().contains("REPLACE_TERM")) {

                    final ICompletion completion =
                        createCompletionReplaceTerm(name, text, additionalInfo, change, blankLineCompletion, "", "");

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                }
            }
        }

        return completions;
    }

    public Collection<ICompletion> listsCompletions(int position, boolean blankLineCompletion,
        Iterable<IStrategoList> lists, String languageName, ILanguageComponent component, FileObject location)
        throws MetaborgException {

        Collection<ICompletion> completions = Lists.newLinkedList();

        final ITermFactory termFactory = termFactoryService.get(component, null, false);

        for(IStrategoList list : lists) {
            ListImploderAttachment attachment = list.getAttachment(null);
            String sort = attachment.getSort().substring(0, attachment.getSort().length() - 1);
            String placeholderName = sort + "-Plhdr";
            IStrategoAppl listPlaceholder = termFactory.makeAppl(termFactory.makeConstructor(placeholderName, 0));
            final IStrategoTerm strategoInput = termFactory.makeTuple(termFactory.makeString(sort), list,
                listPlaceholder, termFactory.makeInt(position));
            final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
            final IStrategoTerm proposalsLists =
                strategoCommon.invoke(runtime, strategoInput, "get-proposals-list-" + languageName);
            if(proposalsLists == null) {
                logger.error("Getting proposals for {} failed", strategoInput);
                continue;
            }
            for(IStrategoTerm proposalTerm : proposalsLists) {
                if(!(proposalTerm instanceof IStrategoTuple)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }
                final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                if(tuple.getSubtermCount() != 4 || !(tuple.getSubterm(0) instanceof IStrategoString)
                    || !(tuple.getSubterm(1) instanceof IStrategoString)
                    || !(tuple.getSubterm(2) instanceof IStrategoString)
                    || !(tuple.getSubterm(3) instanceof IStrategoAppl)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }

                final String name = Tools.asJavaString(tuple.getSubterm(0));
                final String text = Tools.asJavaString(tuple.getSubterm(1));
                final String additionalInfo = Tools.asJavaString(tuple.getSubterm(2));
                final StrategoAppl change = (StrategoAppl) tuple.getSubterm(3);


                // if the change is inserting at the end of a list
                if(change.getConstructor().getName().contains("INSERT_AT_END")) {

                    final ICompletion completion =
                        createCompletionInsertAtEnd(name, text, additionalInfo, change, blankLineCompletion);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                } else if(change.getConstructor().getName().contains("INSERT_BEFORE")) {

                    final ICompletion completion = createCompletionInsertBefore(name, text, additionalInfo, change);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                }
            }
        }

        return completions;
    }

    private ICompletion createCompletionReplaceTerm(String name, String text, String additionalInfo,
        StrategoAppl change, boolean blankLineCompletion, String prefix, String suffix) {

        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);

        if(change.getSubtermCount() != 2 || !(newNode instanceof IStrategoAppl)
            || !(oldNode instanceof IStrategoAppl)) {
            return null;
        }

        final String sort = ImploderAttachment.getSort(oldNode);

        int insertionPoint, suffixPoint;

        final OriginAttachment oldNodeOA = oldNode.getAttachment(OriginAttachment.TYPE);
        final ImploderAttachment oldNodeIA;
        final ITokens tokenizer;
        if (oldNodeOA != null) {
            IStrategoTerm origin = oldNodeOA.getOrigin();
            oldNodeIA = origin.getAttachment(ImploderAttachment.TYPE);
            tokenizer = ImploderAttachment.getTokenizer(origin);
        } else {
            oldNodeIA = oldNode.getAttachment(ImploderAttachment.TYPE);
            tokenizer = ImploderAttachment.getTokenizer(oldNode);
        }
//        final ImploderAttachment oldNodeIA = oldNode.getAttachment(ImploderAttachment.TYPE);
//        ITokens tokenizer = ImploderAttachment.getTokenizer(oldNode);

        // check if it's an empty node
        if(oldNodeIA.getLeftToken().getStartOffset() > oldNodeIA.getRightToken().getEndOffset()) {
            // get the last non-layout token before the new node
            int tokenPosition =
                oldNodeIA.getLeftToken().getIndex() - 1 > 0 ? oldNodeIA.getLeftToken().getIndex() - 1 : 0;
            while(tokenPosition > 0 && (tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR))
                tokenPosition--;
            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();

            // if completion does not spam multiple lines preserve everything starting at the first non-layout char
            if(!additionalInfo.contains("\n")) {
                tokenPosition = oldNodeIA.getLeftToken().getIndex() + 1 < tokenizer.getTokenCount()
                    ? oldNodeIA.getLeftToken().getIndex() + 1 : tokenizer.getTokenCount() - 1;
                while(tokenPosition < tokenizer.getTokenCount()
                    && (tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                        || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR))
                    tokenPosition++;
                suffixPoint = tokenizer.getTokenAt(tokenPosition).getStartOffset();
            } else { // if completion spams multiple lines keep the lines
                suffixPoint = insertionPoint + 1;
            }
            // if completion is triggered in an empty line, consume that line
            IToken checkToken;
            boolean blankLine = false;
            if(blankLineCompletion) {
                for(; tokenPosition < tokenizer.getTokenCount(); tokenPosition++) {
                    checkToken = tokenizer.getTokenAt(tokenPosition);
                    if(tokenizer.toString(checkToken, checkToken).contains("\n")) {
                        suffixPoint = checkToken.getEndOffset();
                        if(!blankLine) {
                            blankLine = true;
                        } else {
                            break;
                        }
                    }
                }
            }


        } else { // if not, do a regular replacement
            insertionPoint = oldNodeIA.getLeftToken().getStartOffset() - 1;
            suffixPoint = oldNodeIA.getRightToken().getEndOffset() + 1;
        }

        CompletionKind kind;

        if(prefix.equals("") && suffix.equals("")) {
            kind = CompletionKind.expansion;
        } else {
            kind = CompletionKind.expansionEditing;
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint, kind, prefix, suffix);
    }

    private ICompletion createCompletionInsertBefore(String name, String text, String additionalInfo,
        StrategoAppl change) {

        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);


        // expect two terms and 1st should be an element of a list
        final StrategoTerm oldList = (StrategoTerm) ParentAttachment.getParent(oldNode);

        if(change.getSubtermCount() != 2 || !(oldNode instanceof IStrategoAppl) || !(newNode instanceof IStrategoList)
            || !(oldList instanceof IStrategoList)) {
            return null;
        }

        final String sort = ImploderAttachment.getSort(oldNode);

        int insertionPoint, suffixPoint;

        ITokens tokenizer = ImploderAttachment.getTokenizer(oldNode);

        IStrategoTerm[] subterms = oldList.getAllSubterms();
        int indexOfElement;
        for(indexOfElement = 0; indexOfElement < subterms.length; indexOfElement++) {
            if(subterms[indexOfElement] == oldNode)
                break;
        }

        // if inserted element is first (only two elements)
        if(indexOfElement == 0) {
            // insert after the first non-layout token before the leftmost token of the
            // completion node
            final ImploderAttachment oldNodeIA = oldNode.getAttachment(ImploderAttachment.TYPE);
            int tokenPosition =
                oldNodeIA.getLeftToken().getIndex() - 1 > 0 ? oldNodeIA.getLeftToken().getIndex() - 1 : 0;
            while((tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR) && tokenPosition > 0)
                tokenPosition--;

            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
        } else {
            // if inserted element is not first
            // insert after at end offset of the rightmost token of the element before the
            // completion
            StrategoTerm elementBefore = (StrategoTerm) oldList.getSubterm(indexOfElement - 1);
            insertionPoint = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken().getEndOffset();
        }

        // if completion is separated by a newline, preserve indentation of the subsequent node
        // else separation follows from the grammar
        String separator = "";
        for(int i = text.length() - 1; i >= 0; i--) {
            if(text.charAt(i) == additionalInfo.charAt(additionalInfo.length() - 1)) {
                break;
            }
            separator = text.charAt(i) + separator;
        }

        IToken checkToken = oldNode.getAttachment(ImploderAttachment.TYPE).getLeftToken();
        int checkTokenIdx = oldNode.getAttachment(ImploderAttachment.TYPE).getLeftToken().getIndex();
        suffixPoint = insertionPoint;
        if(separator.contains("\n")) {
            for(; checkTokenIdx >= 0; checkTokenIdx--) {
                checkToken = tokenizer.getTokenAt(checkTokenIdx);
                if(tokenizer.toString(checkToken, checkToken).contains("\n")) {
                    break;
                }
                suffixPoint = checkToken.getStartOffset();
            }
        } else {
            suffixPoint = checkToken.getStartOffset();
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint,
            CompletionKind.expansion);
    }


    private ICompletion createCompletionInsertAtEnd(String name, String text, String additionalInfo,
        StrategoAppl change, boolean blankLineCompletion) {

        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);

        // expected two lists
        if(change.getSubtermCount() != 2 || !(oldNode instanceof IStrategoList)
            || !(newNode instanceof IStrategoList)) {
            return null;
        }

        final String sort = ImploderAttachment.getElementSort(oldNode);

        int insertionPoint, suffixPoint;

        ITokens tokenizer = ImploderAttachment.getTokenizer(oldNode);
        final ImploderAttachment oldListIA = oldNode.getAttachment(ImploderAttachment.TYPE);
        int tokenPosition;
        // if list is empty
        // insert after the first non-layout token before the leftmost token of the completion
        // node
        if(oldNode.getSubtermCount() == 0) {
            tokenPosition = oldListIA.getLeftToken().getIndex() - 1 > 0 ? oldListIA.getLeftToken().getIndex() - 1 : 0;
            while((tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR) && tokenPosition > 0)
                tokenPosition--;
            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
        } else {
            // if list is not empty
            // insert after at end offset of the rightmost token of the element before the
            // completion
            StrategoTerm elementBefore = (StrategoTerm) oldNode.getSubterm(oldNode.getAllSubterms().length - 1);
            int leftIdx = elementBefore.getAttachment(ImploderAttachment.TYPE).getLeftToken().getIndex();
            int rightIdx = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken().getIndex();
            while((tokenizer.getTokenAt(rightIdx).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(rightIdx).getLength() == 0) && rightIdx > leftIdx) {
                rightIdx--;
            }
            insertionPoint = tokenizer.getTokenAt(rightIdx).getEndOffset();
            tokenPosition = rightIdx;
        }
        suffixPoint = insertionPoint + 1;

        // if completion is triggered in an empty line, consume that line
        IToken checkToken;
        boolean blankLine = false;
        if(blankLineCompletion) {
            for(; tokenPosition < tokenizer.getTokenCount(); tokenPosition++) {
                checkToken = tokenizer.getTokenAt(tokenPosition);
                if(tokenizer.toString(checkToken, checkToken).contains("\n")) {
                    suffixPoint = checkToken.getEndOffset();
                    if(!blankLine) {
                        blankLine = true;
                    } else {
                        break;
                    }
                }
            }
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint,
            CompletionKind.expansion);
    }

    public Collection<ICompletion> completionErroneousPrograms(int cursorPosition,
        Iterable<IStrategoTerm> completionTerms, ISpoofaxParseUnit completionParseResult) throws MetaborgException {

        final FileObject location = completionParseResult.source();
        final ILanguageImpl language = completionParseResult.input().langImpl();
        final String languageName = language.belongsTo().name();
        final Collection<ICompletion> completions = Lists.newLinkedList();
        final Collection<IStrategoTerm> proposalsTerm = Lists.newLinkedList();

        for(ILanguageComponent component : language.components()) {
            final ITermFactory termFactory = termFactoryService.get(component, null, false);
            for(IStrategoTerm completionTerm : completionTerms) {
                IStrategoTerm completionAst = completionParseResult.ast();
                final StrategoTerm topMostAmb = findTopMostAmbNode((StrategoTerm) completionTerm);

                if(ImploderAttachment.get(completionTerm).isSinglePlaceholderCompletion()) {
                    Collection<IStrategoTerm> placeholders = Lists.newLinkedList();
                    placeholders.addAll(findPlaceholderTerms(completionTerm));
                    if(placeholders.size() != 1) {
                        logger.error("Getting proposals for {} failed", completionTerm);
                        continue;
                    }

                    IStrategoAppl placeholderTerm = (IStrategoAppl) Iterables.get(placeholders, 0);
                    IStrategoAppl placeholder = termFactory
                        .makeAppl(termFactory.makeConstructor(placeholderTerm.getConstructor().getName(), 0));


                    IStrategoTerm parenthesized = parenthesizeTerm(completionTerm, termFactory);
                    final IStrategoTerm inputStratego =
                        termFactory.makeTuple(termFactory.makeString(ImploderAttachment.getElementSort(parenthesized)),
                            completionAst, completionTerm, topMostAmb, parenthesized, placeholder, placeholderTerm);

                    final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
                    final IStrategoTerm proposalTerm = strategoCommon.invoke(runtime, inputStratego,
                        "get-proposals-incorrect-programs-single-placeholder-" + languageName);
                    if(proposalTerm == null || !(proposalTerm instanceof IStrategoList)) {
                        logger.error("Getting proposals for {} failed", completionTerm);
                        continue;
                    }

                    for(IStrategoTerm proposalPlaceholder : proposalTerm) {
                        proposalsTerm.add(proposalPlaceholder);
                    }

                } else {

                    IStrategoTerm parenthesized = parenthesizeTerm(completionTerm, termFactory);
                    final IStrategoTerm inputStratego =
                        termFactory.makeTuple(termFactory.makeString(ImploderAttachment.getElementSort(parenthesized)),
                            completionAst, completionTerm, topMostAmb, parenthesized);

                    final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);
                    final IStrategoTerm proposalTerm = strategoCommon.invoke(runtime, inputStratego,
                        "get-proposals-incorrect-programs-" + languageName);
                    if(proposalTerm == null) {
                        logger.error("Getting proposals for {} failed", completionTerm);
                        continue;
                    }

                    proposalsTerm.add(proposalTerm);
                }
            }

            for(IStrategoTerm proposalTerm : proposalsTerm) {
                if(!(proposalTerm instanceof IStrategoTuple)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }
                final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                if(tuple.getSubtermCount() != 6
                    || !(tuple.getSubterm(0) instanceof IStrategoString)
                    || !(tuple.getSubterm(1) instanceof IStrategoString)
                    || !(tuple.getSubterm(2) instanceof IStrategoString)
                    || !(tuple.getSubterm(3) instanceof IStrategoAppl)
                    || (tuple.getSubterm(4) == null)
                    || !(tuple.getSubterm(5) instanceof IStrategoString)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }
                final String name = Tools.asJavaString(tuple.getSubterm(0));
                String text = Tools.asJavaString(tuple.getSubterm(1));
                String additionalInfo = Tools.asJavaString(tuple.getSubterm(2));
                final StrategoAppl change = (StrategoAppl) tuple.getSubterm(3);
                final StrategoTerm completionTerm = (StrategoTerm) tuple.getSubterm(4);
                final String completionKind = Tools.asJavaString(tuple.getSubterm(5));
                String prefix = calculatePrefix(cursorPosition, completionTerm);
                String suffix = calculateSuffix(cursorPosition, completionTerm);

                // if the change is inserting at the end of a list
                if(change.getConstructor().getName().contains("INSERT_AT_END")) {

                    // calls a different method because now, the program has errors that should be fixed
                    final ICompletion completion = createCompletionInsertAtEndFixing(name, text, additionalInfo, prefix,
                        suffix, change, completionKind);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                } else if(change.getConstructor().getName().contains("INSERT_BEFORE")) {

                    final ICompletion completion = createCompletionInsertBeforeFixing(name, text, additionalInfo,
                        prefix, suffix, change, completionKind);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);


                } else if(change.getConstructor().getName().contains("INSERTION_TERM")) {

                    final ICompletion completion = createCompletionInsertionTermFixing(name, text, additionalInfo,
                        prefix, suffix, change, completionKind);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                } else if(change.getConstructor().getName().contains("REPLACE_TERM")) {

                    final ICompletion completion = createCompletionReplaceTermFixing(name, text, additionalInfo, prefix,
                        suffix, change, completionKind);

                    if(completion == null) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    completions.add(completion);
                }
            }
        }

        return completions;
    }

    private String calculatePrefix(int cursorPosition, IStrategoTerm proposalTerm) {

        String prefix = "";
        ITokens tokenizer = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken().getTokenizer();
        IToken leftToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken();
        IToken rightToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getRightToken();
        IToken current = leftToken;
        int endOffsetPrefix = Integer.MIN_VALUE;
        while(current.getEndOffset() < cursorPosition && current != rightToken) {
            if(endOffsetPrefix < current.getEndOffset()) {
                prefix += current.toString();
                endOffsetPrefix = current.getEndOffset();
            }
            current = tokenizer.getTokenAt(current.getIndex() + 1);
        }

        return prefix;
    }


    private String calculateSuffix(int cursorPosition, IStrategoTerm proposalTerm) {

        String suffix = "";
        ITokens tokenizer = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken().getTokenizer();
        IToken leftToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken();
        IToken rightToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getRightToken();
        IToken current = rightToken;
        int startOffsetSuffix = Integer.MAX_VALUE;
        while(current.getStartOffset() >= cursorPosition && current != leftToken) {
            if(startOffsetSuffix > current.getStartOffset()) {
                suffix = current.toString() + suffix;
                startOffsetSuffix = current.getStartOffset();
            }
            current = tokenizer.getTokenAt(current.getIndex() - 1);
        }

        return suffix;
    }

    private ICompletion createCompletionInsertionTermFixing(String name, String text, String additionalInfo,
        String prefix, String suffix, StrategoAppl change, String completionKind) {
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(0);


        if(change.getSubtermCount() != 1 || !(newNode instanceof IStrategoAppl)) {
            return null;
        }

        final String sort = ImploderAttachment.getSort(newNode);

        int insertionPoint, suffixPoint;

        ITokens tokenizer = ImploderAttachment.getTokenizer(newNode);

        final StrategoTerm topMostAmb = findTopMostAmbNode(newNode);
        final ImploderAttachment topMostAmbIA = topMostAmb.getAttachment(ImploderAttachment.TYPE);

        // get the last non-layout token before the topmost ambiguity
        int tokenPosition =
            topMostAmbIA.getLeftToken().getIndex() - 1 > 0 ? topMostAmbIA.getLeftToken().getIndex() - 1 : 0;
        while((tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
            || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR) && tokenPosition > 0)
            tokenPosition--;

        insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();

        if(topMostAmbIA.getRightToken().getEndOffset() < topMostAmbIA.getRightToken().getStartOffset()) {
            // keep all the characters after the last non-layout token if completion ends with a
            // placeholder
            tokenPosition = topMostAmbIA.getRightToken().getIndex();
            while(tokenPosition > 0
                && (tokenizer.getTokenAt(tokenPosition).getEndOffset() < tokenizer.getTokenAt(tokenPosition)
                    .getStartOffset() || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT))
                tokenPosition--;
            suffixPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset() + 1;

        } else {
            // skip all the (erroneous) characters that were in the text already
            suffixPoint = topMostAmbIA.getRightToken().getEndOffset() + 1;
        }

        CompletionKind kind;
        if(completionKind.equals("recovery")) {
            kind = CompletionKind.recovery;
        } else if(completionKind.equals("expansionEditing")) {
            kind = CompletionKind.expansionEditing;
        } else {
            kind = CompletionKind.expansion;
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint, kind, prefix, suffix);
    }


    private ICompletion createCompletionInsertBeforeFixing(String name, String text, String additionalInfo,
        String prefix, String suffix, StrategoAppl change, String completionKind) {

        // expect two terms and 1st should be an element of a list
        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);
        final StrategoTerm oldList = (StrategoTerm) ParentAttachment.getParent(oldNode);

        if(change.getSubtermCount() != 2 || !(oldNode instanceof IStrategoAppl) || !(newNode instanceof IStrategoAppl)
            || !(oldList instanceof IStrategoList)) {
            return null;
        }

        final String sort = ImploderAttachment.getSort(oldNode);

        int insertionPoint, suffixPoint;

        IStrategoTerm[] subterms = ((IStrategoList) oldList).getAllSubterms();
        int indexOfCompletion;
        for(indexOfCompletion = 0; indexOfCompletion < subterms.length; indexOfCompletion++) {
            if(subterms[indexOfCompletion] == oldNode)
                break;
        }
        // if inserted element is first (only two elements)
        if(indexOfCompletion == 1) {
            // insert after the first non-layout token before the leftmost token of the list
            ITokens tokenizer = ImploderAttachment.getTokenizer(oldList);

            // to avoid keeping duplicate tokens due to ambiguity
            IStrategoTerm topMostAmbOldList = findTopMostAmbNode(oldList);
            final ImploderAttachment oldListIA = topMostAmbOldList.getAttachment(ImploderAttachment.TYPE);

            int tokenPosition =
                oldListIA.getLeftToken().getIndex() - 1 > 0 ? oldListIA.getLeftToken().getIndex() - 1 : 0;
            while((checkEmptyOffset(tokenizer.getTokenAt(tokenPosition))
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR) && tokenPosition > 0)
                tokenPosition--;

            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
        } else {
            // if inserted element is not first
            // insert after at end offset of the rightmost token of the element before the completion
            StrategoTerm elementBefore = (StrategoTerm) oldList.getSubterm(indexOfCompletion - 2);
            insertionPoint = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken().getEndOffset();

        }
        // suffix point should be the first token of the next element
        suffixPoint = oldNode.getAttachment(ImploderAttachment.TYPE).getLeftToken().getStartOffset();

        CompletionKind kind;
        if(completionKind.equals("recovery")) {
            kind = CompletionKind.recovery;
        } else if(completionKind.equals("expansionEditing")) {
            kind = CompletionKind.expansionEditing;
        } else {
            kind = CompletionKind.expansion;
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint, kind, prefix, suffix);
    }


    private ICompletion createCompletionInsertAtEndFixing(String name, String text, String additionalInfo,
        String prefix, String suffix, StrategoAppl change, String completionKind) {

        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);

        final StrategoTerm oldNodeTopMostAmb = findTopMostAmbNode(oldNode);


        if(change.getSubtermCount() != 2 || !(oldNode instanceof IStrategoList)
            || !(newNode instanceof IStrategoAppl)) {
            return null;
        }

        final String sort = ImploderAttachment.getElementSort(oldNode);

        int insertionPoint, suffixPoint;
        ITokens tokenizer = ImploderAttachment.getTokenizer(oldNodeTopMostAmb);
        final ImploderAttachment oldNodeIA = oldNodeTopMostAmb.getAttachment(ImploderAttachment.TYPE);

        // if list is empty
        // insert after the first non-layout token before the leftmost token of the completion node
        if(((IStrategoList) oldNode).size() == 1) {
            int tokenPosition =
                oldNodeIA.getLeftToken().getIndex() - 1 > 0 ? oldNodeIA.getLeftToken().getIndex() - 1 : 0;
            while(tokenPosition > 0 && (checkEmptyOffset(tokenizer.getTokenAt(tokenPosition))
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR))
                tokenPosition--;
            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
        } else {
            // if list is not empty
            // insert after at end offset of the rightmost token of the element before the completion
            StrategoTerm elementBefore = (StrategoTerm) oldNode.getSubterm(oldNode.getAllSubterms().length - 2);
            insertionPoint = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken().getEndOffset();
        }

        // keep all the characters after the last non-layout token
        int tokenPosition = oldNodeIA.getRightToken().getIndex();
        while(tokenizer.getTokenAt(tokenPosition).getEndOffset() < tokenizer.getTokenAt(tokenPosition).getStartOffset()
            || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT && tokenPosition > 0)
            tokenPosition--;
        suffixPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset() + 1;


        CompletionKind kind;
        if(completionKind.equals("recovery")) {
            kind = CompletionKind.recovery;
        } else if(completionKind.equals("expansionEditing")) {
            kind = CompletionKind.expansionEditing;
        } else {
            kind = CompletionKind.expansion;
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint, kind, prefix, suffix);
    }

    private boolean isCompletionNode(ISimpleTerm term) {
        if(term == null)
            return false;

        if(ImploderAttachment.get(term).isCompletion() || ImploderAttachment.get(term).isNestedCompletion())
            return true;

        return false;
    }

    public Collection<? extends ICompletion> completionErroneousProgramsNested(int cursorPosition,
        Collection<IStrategoTerm> nestedCompletionTerms, ISpoofaxParseUnit completionParseResult)
        throws MetaborgException {
        final FileObject location = completionParseResult.source();
        final ILanguageImpl language = completionParseResult.input().langImpl();
        final String languageName = language.belongsTo().name();
        final Collection<ICompletion> completions = Lists.newLinkedList();
        IStrategoTerm completionAst = completionParseResult.ast();

        for(ILanguageComponent component : language.components()) {
            final ITermFactory termFactory = termFactoryService.get(component, null, false);
            for(IStrategoTerm nestedCompletionTerm : nestedCompletionTerms) {
                final HybridInterpreter runtime = strategoRuntimeService.runtime(component, location, false);

                Collection<IStrategoTerm> inputsStrategoNested = Lists.newLinkedList();

                // calculate direct proposals
                inputsStrategoNested.addAll(calculateDirectCompletionProposals(nestedCompletionTerm, termFactory,
                    completionAst, languageName, runtime));

                // calculate inner nested proposals
                Collection<IStrategoTerm> innerNestedCompletionTerms =
                    findNestedCompletionTerm((StrategoTerm) nestedCompletionTerm, true);

                for(IStrategoTerm innerNestedCompletionTerm : innerNestedCompletionTerms) {
                    inputsStrategoNested.addAll(calculateNestedCompletionProposals(nestedCompletionTerm,
                        innerNestedCompletionTerm, termFactory, completionAst, languageName, runtime));
                }

                for(IStrategoTerm inputStrategoNested : inputsStrategoNested) {
                    final IStrategoTerm proposalTermNested = strategoCommon.invoke(runtime, inputStrategoNested,
                        "get-proposals-incorrect-programs-nested-" + languageName);
                    if(proposalTermNested == null) {
                        logger.error("Getting proposals for {} failed", inputStrategoNested);
                        continue;
                    }

                    final String name = Tools.asJavaString(proposalTermNested.getSubterm(0));
                    final String text = Tools.asJavaString(proposalTermNested.getSubterm(1));
                    final String additionalInfo = Tools.asJavaString(proposalTermNested.getSubterm(2));
                    final StrategoAppl change = (StrategoAppl) proposalTermNested.getSubterm(3);
                    final StrategoTerm completionTerm = (StrategoTerm) proposalTermNested.getSubterm(4);
                    String prefix = calculatePrefix(cursorPosition, completionTerm);
                    String suffix = calculateSuffix(cursorPosition, completionTerm);
                    String completionKind = "recovery";

                    // if the change is inserting at the end of a list
                    if(change.getConstructor().getName().contains("INSERT_AT_END")) {

                        // calls a different method because now, the program has errors that should be fixed
                        final ICompletion completion = createCompletionInsertAtEndFixing(name, text, additionalInfo,
                            prefix, suffix, change, completionKind);

                        if(completion == null) {
                            logger.error("Unexpected proposal term {}, skipping", proposalTermNested);
                            continue;
                        }

                        completions.add(completion);
                    } else if(change.getConstructor().getName().contains("INSERT_BEFORE")) {

                        final ICompletion completion = createCompletionInsertBeforeFixing(name, text, additionalInfo,
                            prefix, suffix, change, completionKind);

                        if(completion == null) {
                            logger.error("Unexpected proposal term {}, skipping", proposalTermNested);
                            continue;
                        }

                        completions.add(completion);


                    } else if(change.getConstructor().getName().contains("INSERTION_TERM")) {

                        final ICompletion completion = createCompletionInsertionTermFixing(name, text, additionalInfo,
                            prefix, suffix, change, completionKind);

                        if(completion == null) {
                            logger.error("Unexpected proposal term {}, skipping", proposalTermNested);
                            continue;
                        }

                        completions.add(completion);
                    } else if(change.getConstructor().getName().contains("REPLACE_TERM")) {

                        final ICompletion completion = createCompletionReplaceTermFixing(name, text, additionalInfo,
                            prefix, suffix, change, completionKind);

                        if(completion == null) {
                            logger.error("Unexpected proposal term {}, skipping", proposalTermNested);
                            continue;
                        }

                        completions.add(completion);
                    }
                }
            }
        }

        return completions;
    }


    private Collection<IStrategoTerm> calculateNestedCompletionProposals(IStrategoTerm mainNestedCompletionTerm,
        IStrategoTerm nestedCompletionTerm, ITermFactory termFactory, IStrategoTerm completionAst, String languageName,
        HybridInterpreter runtime) throws MetaborgException {
        Collection<IStrategoTerm> inputsStratego = Lists.newLinkedList();

        Collection<IStrategoTerm> nestedCompletionTerms =
            findNestedCompletionTerm((StrategoTerm) nestedCompletionTerm, true);

        for(IStrategoTerm innerNestedCompletionTerm : nestedCompletionTerms) {
            Collection<IStrategoTerm> inputsStrategoInnerNested = calculateNestedCompletionProposals(
                nestedCompletionTerm, innerNestedCompletionTerm, termFactory, completionAst, languageName, runtime);
            for(IStrategoTerm inputStrategoNested : inputsStrategoInnerNested) {
                final IStrategoTerm proposalTermNested = strategoCommon.invoke(runtime, inputStrategoNested,
                    "get-proposals-incorrect-programs-nested-" + languageName);
                if(proposalTermNested == null) {
                    logger.error("Getting proposals for {} failed", inputStrategoNested);
                    continue;
                }
                final StrategoTerm topMostAmb = findTopMostAmbNode((StrategoTerm) nestedCompletionTerm);
                final IStrategoTerm replaceTermText = termFactory.makeAppl(
                    new StrategoConstructor("REPLACE_TERM_TEXT", 2), topMostAmb, proposalTermNested.getSubterm(1));

                IStrategoTerm parenthesized = parenthesizeTerm(mainNestedCompletionTerm, termFactory);
                final IStrategoTerm inputStrategoInnerNested = termFactory.makeTuple(
                    termFactory.makeString(ImploderAttachment.getElementSort(parenthesized)), completionAst,
                    mainNestedCompletionTerm, proposalTermNested.getSubterm(0), replaceTermText, parenthesized);

                inputsStratego.add(inputStrategoInnerNested);
            }

        }

        Collection<IStrategoTerm> inputsStrategoInner =
            calculateDirectCompletionProposals(nestedCompletionTerm, termFactory, completionAst, languageName, runtime);

        for(IStrategoTerm inputStrategoNested : inputsStrategoInner) {
            final IStrategoTerm proposalTermNested = strategoCommon.invoke(runtime, inputStrategoNested,
                "get-proposals-incorrect-programs-nested-" + languageName);
            if(proposalTermNested == null) {
                logger.error("Getting proposals for {} failed", inputStrategoNested);
                continue;
            }
            final StrategoTerm topMostAmb = findTopMostAmbNode((StrategoTerm) nestedCompletionTerm);
            final IStrategoTerm replaceTermText = termFactory.makeAppl(new StrategoConstructor("REPLACE_TERM_TEXT", 2),
                topMostAmb, proposalTermNested.getSubterm(1));

            IStrategoTerm parenthesized = parenthesizeTerm(mainNestedCompletionTerm, termFactory);
            final IStrategoTerm inputStrategoInnerNested = termFactory.makeTuple(
                termFactory.makeString(ImploderAttachment.getElementSort(parenthesized)), completionAst,
                mainNestedCompletionTerm, proposalTermNested.getSubterm(0), replaceTermText, parenthesized);

            inputsStratego.add(inputStrategoInnerNested);
        }

        return inputsStratego;
    }


    private Collection<IStrategoTerm> calculateDirectCompletionProposals(IStrategoTerm nestedCompletionTerm,
        ITermFactory termFactory, IStrategoTerm completionAst, String languageName, HybridInterpreter runtime)
        throws MetaborgException {

        Collection<IStrategoTerm> inputsStratego = Lists.newLinkedList();
        Collection<IStrategoTerm> completionTerms = findCompletionTermInsideNested((StrategoTerm) nestedCompletionTerm);

        for(IStrategoTerm completionTerm : completionTerms) {
            final StrategoTerm topMostCompletionTerm = findTopMostCompletionNode((StrategoTerm) completionTerm);
            final StrategoTerm topMostAmb = findTopMostAmbNode(topMostCompletionTerm);

            IStrategoTerm parenthesized = parenthesizeTerm(topMostCompletionTerm, termFactory);

            final IStrategoTerm inputStratego =
                termFactory.makeTuple(termFactory.makeString(ImploderAttachment.getElementSort(parenthesized)),
                    completionAst, completionTerm, topMostAmb, parenthesized);

            final IStrategoTerm proposalTerm =
                strategoCommon.invoke(runtime, inputStratego, "get-proposals-incorrect-programs-" + languageName);
            if(proposalTerm == null) {
                logger.error("Getting proposals for {} failed", inputStratego);
                continue;
            }

            final IStrategoTerm replaceTermText = termFactory.makeAppl(new StrategoConstructor("REPLACE_TERM_TEXT", 2),
                topMostAmb, proposalTerm.getSubterm(1));

            IStrategoTerm parenthesizedNested = parenthesizeTerm(nestedCompletionTerm, termFactory);
            final IStrategoTerm inputStrategoNested = termFactory.makeTuple(
                termFactory.makeString(ImploderAttachment.getElementSort(parenthesizedNested)), completionAst,
                nestedCompletionTerm, proposalTerm.getSubterm(0), replaceTermText, parenthesizedNested);

            inputsStratego.add(inputStrategoNested);
        }

        return inputsStratego;
    }

    private IStrategoTerm parenthesizeTerm(IStrategoTerm completionTerm, ITermFactory termFactory) {
        if(ImploderAttachment.get(completionTerm).isBracket()) {
            IStrategoTerm result =
                termFactory.makeAppl(termFactory.makeConstructor("Parenthetical", 1), completionTerm);
            return result;
        }
        return completionTerm;
    }


    private ICompletion createCompletionReplaceTermFixing(String name, String text, String additionalInfo,
        String prefix, String suffix, StrategoAppl change, String completionKind) {
        final StrategoTerm oldNode = (StrategoTerm) change.getSubterm(0);
        final StrategoTerm newNode = (StrategoTerm) change.getSubterm(1);


        if(change.getSubtermCount() != 2 || !(newNode instanceof IStrategoAppl)
            || !(oldNode instanceof IStrategoAppl)) {
            return null;
        }

        final String sort = ImploderAttachment.getSort(oldNode);

        int insertionPoint, suffixPoint;

        final ImploderAttachment oldNodeIA = oldNode.getAttachment(ImploderAttachment.TYPE);
        ITokens tokenizer = ImploderAttachment.getTokenizer(oldNode);

        // check if it's an empty node
        if(oldNodeIA.getLeftToken().getStartOffset() > oldNodeIA.getRightToken().getEndOffset()) {
            // get the last non-layout token before the new node
            int tokenPosition =
                oldNodeIA.getLeftToken().getIndex() - 1 > 0 ? oldNodeIA.getLeftToken().getIndex() - 1 : 0;
            while((tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_LAYOUT
                || tokenizer.getTokenAt(tokenPosition).getKind() == IToken.TK_ERROR) && tokenPosition > 0)
                tokenPosition--;
            insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
        } else { // if not, do a regular replacement
            insertionPoint = oldNodeIA.getLeftToken().getStartOffset() - 1;
        }


        // insert after the first non-layout token
        int tokenPositionEnd = oldNodeIA.getRightToken().getIndex();

        while((tokenizer.getTokenAt(tokenPositionEnd).getEndOffset() < tokenizer.getTokenAt(tokenPositionEnd)
            .getStartOffset() || tokenizer.getTokenAt(tokenPositionEnd).getKind() == IToken.TK_LAYOUT
            || tokenizer.getTokenAt(tokenPositionEnd).getKind() == IToken.TK_ERROR) && tokenPositionEnd > 0)
            tokenPositionEnd--;

        suffixPoint = tokenizer.getTokenAt(tokenPositionEnd).getEndOffset() + 1;

        CompletionKind kind;
        if(completionKind.equals("recovery")) {
            kind = CompletionKind.recovery;
        } else {
            kind = CompletionKind.expansion;
        }

        return new Completion(name, sort, text, additionalInfo, insertionPoint + 1, suffixPoint, kind, prefix, suffix);
    }

    private boolean checkEmptyOffset(IToken token) {
        if(token.getStartOffset() > token.getEndOffset())
            return true;

        return false;
    }

    private @Nullable IStrategoAppl getPlaceholder(int position, final Iterable<IStrategoTerm> terms) {
        for(IStrategoTerm term : terms) {
            if(term instanceof IStrategoAppl) {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);

                final IStrategoAppl appl = (IStrategoAppl) term;
                if(appl.getConstructor().getName().endsWith("-Plhdr") && position > left.getStartOffset()
                    && position <= right.getEndOffset()) {
                    return appl;
                }
            }
        }

        return null;
    }

    private @Nullable Iterable<IStrategoList> getLists(final Iterable<IStrategoTerm> terms,
        Map<IStrategoTerm, Boolean> leftRecursiveTerms, Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoList> lists = Lists.newLinkedList();
        for(IStrategoTerm term : terms) {
            if(term instanceof IStrategoList) {
                final IStrategoList list = (IStrategoList) term;
                lists.add(list);
            } else {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);
                // if term is not nullable, nor a list nor left or right recursive stop the search
                if(left.getStartOffset() <= right.getEndOffset()) {
                    boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);
                    boolean isRightRecursive = rightRecursiveTerms.containsKey(term);
                    if(!isLeftRecursive && !isRightRecursive) {
                        break;
                    }
                }
            }
        }

        return lists;
    }

    private @Nullable Iterable<IStrategoTerm> getOptionals(final Iterable<IStrategoTerm> terms,
        Map<IStrategoTerm, Boolean> leftRecursiveTerms, Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoTerm> optionals = Lists.newLinkedList();
        for(IStrategoTerm term : terms) {
            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);
            if(!(term instanceof IStrategoList) && left.getStartOffset() > right.getEndOffset()) {
                optionals.add(term);
            } else if(term instanceof IStrategoList) {
                continue;
                // if term is not nullable, nor a list nor left or right recursive stop the search
            } else {
                boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);
                boolean isRightRecursive = rightRecursiveTerms.containsKey(term);
                if(!isLeftRecursive && !isRightRecursive) {
                    break;
                }
            }
        }

        return optionals;
    }

    private Iterable<IStrategoTerm> getRightRecursiveTerms(int position, Iterable<IStrategoTerm> terms,
        Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoTerm> rightRecursive = Lists.newLinkedList();
        for(IStrategoTerm term : terms) {
            boolean isRightRecursive = rightRecursiveTerms.containsKey(term);

            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);

            if(isRightRecursive && position <= left.getStartOffset()) {
                rightRecursive.add(term);
            } else if(term instanceof IStrategoList || left.getStartOffset() > right.getEndOffset()) {
                continue;
            } else {
                break;
            }
        }

        return rightRecursive;
    }

    private Iterable<IStrategoTerm> getLeftRecursiveTerms(int position, Iterable<IStrategoTerm> terms,
        Map<IStrategoTerm, Boolean> leftRecursiveTerms) {
        Collection<IStrategoTerm> leftRecursive = Lists.newLinkedList();
        for(IStrategoTerm term : terms) {
            boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);

            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);

            if(isLeftRecursive && position > right.getEndOffset()) {
                leftRecursive.add(term);
            } else if(term instanceof IStrategoList || left.getStartOffset() > right.getEndOffset()) {
                continue;
            } else {
                break;
            }
        }

        return leftRecursive;
    }

    private Collection<IStrategoTerm> getCompletionTermsFromAST(ISpoofaxParseUnit completionParseResult) {

        if(completionParseResult == null) {
            return Lists.newLinkedList();
        }

        StrategoTerm ast = (StrategoTerm) completionParseResult.ast();

        if(ast == null) {
            return Lists.newLinkedList();
        }

        Collection<IStrategoTerm> completionTerm = findCompletionTerm(ast);

        return completionTerm;
    }

    private Collection<IStrategoTerm> getNestedCompletionTermsFromAST(ISpoofaxParseUnit completionParseResult) {
        if(completionParseResult == null) {
            return Lists.newLinkedList();
        }

        StrategoAppl ast = (StrategoAppl) completionParseResult.ast();

        if(ast == null) {
            return Lists.newLinkedList();
        }

        Collection<IStrategoTerm> completionTerm = findNestedCompletionTerm(ast, false);

        return completionTerm;
    }

    private Iterable<IStrategoTerm> tracingTermsCompletions(final int position, Object result,
        final ISourceRegion region, final HybridInterpreter runtime, final ITermFactory termFactory,
        final String languageName, final Map<IStrategoTerm, Boolean> leftRecursiveTerms,
        final Map<IStrategoTerm, Boolean> rightRecursiveTerms) {
        if(result == null || region == null) {
            return Iterables2.empty();
        }
        final Collection<IStrategoTerm> parsed = Lists.newLinkedList();

        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {
            @Override public boolean visit(IStrategoTerm term) {
                final ISourceLocation location = fromTokens(term, runtime, termFactory, position, languageName,
                    leftRecursiveTerms, rightRecursiveTerms);
                if(location != null && location.region().contains(region)) {
                    parsed.add(term);
                    return false;
                }
                return true;
            }
        };

        StrategoTermVisitee.bottomup(visitor, (IStrategoTerm) result);
        return parsed;
    }

    /**
     * Returns the terms at the specified offset, from leaf to root.
     *
     * @param ast The AST.
     * @param caretOffset The caret offset.
     * @param runtime The hybrid interpreter.
     * @param termFactory The term factory.
     * @param leftRecursiveTerms A map to which left-recursive terms are added.
     * @param rightRecursiveTerms A map to which right-recursive terms are added.
     * @return The list of terms at the offset, ordered from leaf to root.
     */
    private List<IStrategoTerm> findAstTermsAtOffset(
            IStrategoTerm ast,
            final int caretOffset,
            final HybridInterpreter runtime,
            final ITermFactory termFactory,
            final Map<IStrategoTerm, Boolean> leftRecursiveTerms,
            final Map<IStrategoTerm, Boolean> rightRecursiveTerms) {
        if (ast == null) {
            return Collections.emptyList();
        }
        SourceRegion region = new SourceRegion(caretOffset);
        final List<IStrategoTerm> parsed = Lists.newLinkedList();
        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {
            @Override public boolean visit(IStrategoTerm term) {
                final ISourceLocation location = getFragmentExtent(term, caretOffset, runtime, termFactory,
                        leftRecursiveTerms, rightRecursiveTerms);
                if(location != null && location.region().contains(region)) {
                    parsed.add(term);
                    return false;
                }
                return true;
            }
        };
        StrategoTermVisitee.bottomup(visitor, ast);
        return parsed;
    }

    /**
     * Gets the extent of an AST fragment in the source text.
     *
     * @param fragment The AST fragment.
     * @param caretOffset The caret offset.
     * @param runtime The hybrid interpreter.
     * @param termFactory The term factory.
     * @param leftRecursiveTerms A map to which left-recursive terms are added.
     * @param rightRecursiveTerms A map to which right-recursive terms are added.
     * @return The extend of the source fragment.
     */
    @Nullable
    private ISourceLocation getFragmentExtent(
            IStrategoTerm fragment,
            int caretOffset,
            HybridInterpreter runtime,
            ITermFactory termFactory,
            Map<IStrategoTerm, Boolean> leftRecursiveTerms,
            Map<IStrategoTerm, Boolean> rightRecursiveTerms) {
        IToken left = ImploderAttachment.getLeftToken(fragment);
        IToken right = ImploderAttachment.getRightToken(fragment);
        boolean isLeftRecursive = fragment instanceof IStrategoAppl
                && caretOffset > right.getEndOffset()
                && isRecursive(fragment, "is-left-recursive", runtime, termFactory);
        boolean isRightRecursive = fragment instanceof IStrategoAppl
                && caretOffset <= left.getStartOffset()
                && isRecursive(fragment, "is-right-recursive", runtime, termFactory);
        if (isLeftRecursive) {
            leftRecursiveTerms.put(fragment, true);
        }
        if (isRightRecursive) {
            rightRecursiveTerms.put(fragment, true);
        }
        if (left == null || right == null) {
            return null;
        }
        boolean isList = fragment instanceof IStrategoList;
        boolean isOptional = !isList && left == right && left.getEndOffset() < left.getStartOffset();
        boolean isEmpty = left.getStartOffset() > right.getEndOffset();
        boolean isNullable = isOptional || isList || isLeftRecursive || isRightRecursive;
        ITokens tokenizer = ImploderAttachment.getTokenizer(fragment);
        // If it's a list, empty node, optional node, or right-recursive, include the layout to the left.
        if (isEmpty || isOptional || isList || isRightRecursive) {
            // Include layout to the left
            left = includeLayout(tokenizer, left, -1);
        } else {
            // Exclude layout from the left
            left = excludeLayout(tokenizer, left, right, +1);
        }
        // If it's a list, empty node, optional node, or left-recursive, include the layout to the right.
        if (isEmpty || isOptional || isList || isLeftRecursive) {
            // Include layout to the right
            right = includeLayout(tokenizer, right, +1);
        } else {
            // Exclude layout from the right
            right = excludeLayout(tokenizer, right, left, -1);
        }
        final FileObject resource = SourceAttachment.getResource(fragment, this.resourceService);
        final ISourceRegion region = JSGLRSourceRegionFactory.fromTokensLayout(left, right, isNullable);
        return new SourceLocation(region, resource);
    }

    /**
     * Includes any layout from the specified token in the specified direction,
     * and returns the new token.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token.
     */
    private IToken includeLayout(ITokens tokenizer, IToken token, int direction) {
        int index = includeLayoutByIndex(tokenizer, token, direction);
        return tokenizer.getTokenAt(index);
    }

    /**
     * Includes any layout from the specified token in the specified direction,
     * and returns the token index of the new token.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token index.
     */
    private int includeLayoutByIndex(ITokens tokenizer, IToken token, int direction) {
        int tokenIndex = token.getIndex();
        for(int i = token.getIndex() + direction; i >= 0 && i < tokenizer.getTokenCount(); i += direction) {
            if (tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                tokenIndex = i;
            } else {
                break;
            }
        }
        return tokenIndex;
    }

    /**
     * Excludes any layout from the specified token in the specified direction up to the specified end token,
     * and returns the new token.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param end The token where to end.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token.
     */
    private IToken excludeLayout(ITokens tokenizer, IToken token, IToken end, int direction) {
        int index = excludeLayoutByIndex(tokenizer, token, end, direction);
        return tokenizer.getTokenAt(index);
    }

    /**
     * Excludes any layout from the specified token in the specified direction up to the specified end token,
     * and returns the token index of the new token.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param end The token where to end.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token index.
     */
    private int excludeLayoutByIndex(ITokens tokenizer, IToken token, IToken end, int direction) {
        int tokenIndex = token.getIndex();
        for(int i = token.getIndex(); i != end.getIndex(); i += direction) {
            if (tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                tokenIndex = i + direction;
            } else {
                break;
            }
        }
        return tokenIndex;
    }

    /**
     * Determines whether the specified fragment is left or right recursive.
     *
     * @param fragment The AST fragment.
     * @param strategyName The strategy that, when it succeeds, determines whether the fragment is recursive.
     * @param runtime The hybrid interpreter to use.
     * @param termFactory The term factory to use.
     * @return True when the term is recursive, otherwise, False.
     */
    private boolean isRecursive(IStrategoTerm fragment,
                                String strategyName,
                                final HybridInterpreter runtime,
                                final ITermFactory termFactory) {
        String sort = ImploderAttachment.getSort(fragment);
        IStrategoTerm input = termFactory.makeString(sort);
        try {
            // The strategy should not fail (i.e. return null).
            return this.strategoCommon.invoke(runtime, input, strategyName) != null;
        } catch(MetaborgException e) {
            logger.error("Failed to check recursivity for term {} of sort {} - syntactic completion not activated " +
                            "for this language, please import the completion Stratego library",
                    fragment, sort);
        }
        return false;
    }

    /**
     * Gets the first placeholder term from the list of terms.
     *
     * When the list is ordered from leaf to root, this returns the deepest term, if any.
     *
     * @param terms The list of terms.
     * @param caretOffset The caret offset.
     * @return The first placeholder term; or null when not found.
     */
    @Nullable
    private IStrategoAppl getPlaceholderFromTerms(
            final Iterable<IStrategoTerm> terms,
            int caretOffset) {
        for (IStrategoTerm term : terms) {
            if (isPlaceholderTerm(term)) {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);
                if (caretOffset > left.getStartOffset() &&
                        caretOffset <= right.getEndOffset()) {
                    return (IStrategoAppl)term;
                }
            }
        }
        return null;
    }

    /**
     * Gets the term that contains the specified term as a direct child.
     *
     * @param term The term to look for.
     * @param candidates The candidates in which to search.
     * @return The parent term; or null when not found.
     */
    @Nullable
    private IStrategoTerm getParentTermOf(IStrategoTerm term, Iterable<IStrategoTerm> candidates) {
        for(IStrategoTerm candidate : candidates) {
            for (IStrategoTerm candidateChild : candidate.getAllSubterms()) {
                if (candidateChild == term) {
                    return candidate;
                }
            }
        }
        return null;
    }

    /**
     * Determines whether the specified term is a placeholder term.
     *
     * @param term The term to check.
     * @return True when the term is a placeholder term; otherwise, false.
     */
    private boolean isPlaceholderTerm(IStrategoTerm term) {
        // A placeholder is a term whose constructor ends with "-Plhdr".
        return term instanceof IStrategoAppl
                && ((IStrategoAppl)term).getConstructor().getName().endsWith(PLACEHOLDER_SORT_SUFFIX);
    }

    /**
     * Gets the name of the placeholder sort.
     *
     * @param sort The sort name.
     * @return The placeholder sort name.
     */
    private static String getPlaceholderSortName(String sort) {
        return sort + PLACEHOLDER_SORT_SUFFIX;
    }

    // TODO: Do these strategies need to be specific for a language
    protected @Nullable ISourceLocation fromTokens(IStrategoTerm fragment, HybridInterpreter runtime,
        ITermFactory termFactory, int position, String languageName, Map<IStrategoTerm, Boolean> leftRecursiveTerms,
        Map<IStrategoTerm, Boolean> rightRecursiveTerms) {
        final FileObject resource = SourceAttachment.getResource(fragment, resourceService);
        final IToken left = ImploderAttachment.getLeftToken(fragment);
        final IToken right = ImploderAttachment.getRightToken(fragment);
        ITokens tokenizer = ImploderAttachment.getTokenizer(fragment);
        IToken leftmostValid = left;
        IToken rightmostValid = right;
        boolean isList = (fragment instanceof IStrategoList) ? true : false;
        boolean isOptional = false;
        String sort = ImploderAttachment.getSort(fragment);
        IStrategoTerm input = termFactory.makeString(sort);
        boolean isLeftRecursive = false;

        if(fragment instanceof IStrategoAppl && position > right.getEndOffset()) {
            try {
                isLeftRecursive = strategoCommon.invoke(runtime, input, "is-left-recursive") != null;
            } catch(MetaborgException e) {
                logger.error(
                    "Failed to check recursivity for term {} of sort {} - syntactic completion not activated for this language, please import the completion stratego library",
                    fragment, sort);
            }
        }
        boolean isRightRecursive = false;

        if(fragment instanceof IStrategoAppl && position <= left.getStartOffset()) {
            try {
                isRightRecursive = strategoCommon.invoke(runtime, input, "is-right-recursive") != null;
            } catch(MetaborgException e) {
                logger.error(
                    "Failed to check recursivity for term {} of sort {} - syntactic completion not activated for this language, please import the completion stratego library",
                    fragment, sort);
            }
        }

        if(isLeftRecursive) {
            leftRecursiveTerms.put(fragment, true);
        }

        if(isRightRecursive) {
            rightRecursiveTerms.put(fragment, true);
        }

        if(left == null || right == null) {
            return null;
        }

        if(!isList && left == right && left.getEndOffset() < left.getStartOffset()) {
            isOptional = true;
        }

        // if it's a list or a node that is empty make the element includes the surrounding layout tokens
        if(left.getStartOffset() > right.getEndOffset() || isList || isOptional
            || (isLeftRecursive && isRightRecursive)) {
            for(int i = left.getIndex() - 1; i >= 0; i--) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    leftmostValid = tokenizer.getTokenAt(i);
                } else {
                    break;
                }
            }

            for(int i = right.getIndex() + 1; i < tokenizer.getTokenCount(); i++) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    rightmostValid = tokenizer.getTokenAt(i);
                } else {
                    break;
                }
            }
            // if it is left recursive include the layout only on the right
        } else if(isLeftRecursive) {
            for(int i = left.getIndex(); i < right.getIndex(); i++) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    leftmostValid = tokenizer.getTokenAt(i + 1);
                } else {
                    break;
                }
            }

            for(int i = right.getIndex() + 1; i < tokenizer.getTokenCount(); i++) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    rightmostValid = tokenizer.getTokenAt(i);
                } else {
                    break;
                }
            }

            // if it is right recursive include the layout only on the left
        } else if(isRightRecursive) {
            for(int i = left.getIndex() - 1; i >= 0; i--) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    leftmostValid = tokenizer.getTokenAt(i);
                } else {
                    break;
                }
            }

            for(int i = right.getIndex(); i > left.getIndex(); i--) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    rightmostValid = tokenizer.getTokenAt(i - 1);
                } else {
                    break;
                }
            }

        }
        // if not make it stripes the surrounding layout tokens
        else {
            for(int i = left.getIndex(); i < right.getIndex(); i++) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    leftmostValid = tokenizer.getTokenAt(i + 1);
                } else {
                    break;
                }
            }

            for(int i = right.getIndex(); i > left.getIndex(); i--) {
                if(tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF
                    || tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                    rightmostValid = tokenizer.getTokenAt(i - 1);
                } else {
                    break;
                }
            }
        }

        final ISourceRegion region = JSGLRSourceRegionFactory.fromTokensLayout(leftmostValid, rightmostValid,
            (isOptional || isList || isLeftRecursive || isRightRecursive));

        return new SourceLocation(region, resource);
    }

    private Collection<IStrategoTerm> findCompletionTerm(StrategoTerm ast) {


        final Collection<IStrategoTerm> completionTerms = Lists.newLinkedList();
        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {

            @Override public boolean visit(IStrategoTerm term) {
                ImploderAttachment ia = term.getAttachment(ImploderAttachment.TYPE);
                if(ia.isNestedCompletion()) {
                    return false;
                }
                if(ia.isCompletion()) {
                    completionTerms.add(term);
                    return false;
                }
                return true;
            }
        };
        StrategoTermVisitee.topdown(visitor, ast);

        return completionTerms;
    }

    private Collection<IStrategoTerm> findPlaceholderTerms(IStrategoTerm ast) {

        final Collection<IStrategoTerm> placeholderTerms = Lists.newLinkedList();
        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {

            @Override public boolean visit(IStrategoTerm term) {
                if(term instanceof IStrategoAppl) {
                    IStrategoAppl appl = (IStrategoAppl) term;
                    if(appl.getConstructor().getName().contains("-Plhdr") && appl.getSubtermCount() > 0) {
                        placeholderTerms.add(appl);
                        return false;
                    }
                }
                return true;
            }
        };
        StrategoTermVisitee.topdown(visitor, ast);

        return placeholderTerms;
    }

    private Collection<IStrategoTerm> findCompletionTermInsideNested(final StrategoTerm ast) {

        final Collection<IStrategoTerm> completionTerms = Lists.newLinkedList();
        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {

            @Override public boolean visit(IStrategoTerm term) {
                ImploderAttachment ia = term.getAttachment(ImploderAttachment.TYPE);
                if(ia.isNestedCompletion() && !term.equals(ast)) {
                    return false;
                }
                if(ia.isCompletion()) {
                    completionTerms.add(term);
                    return false;
                }
                return true;
            }
        };
        StrategoTermVisitee.topdown(visitor, ast);

        return completionTerms;
    }

    private Collection<IStrategoTerm> findNestedCompletionTerm(final StrategoTerm ast, final boolean excludeIdTerm) {
        final Collection<IStrategoTerm> completionTerms = Lists.newLinkedList();
        final IStrategoTermVisitor visitor = new AStrategoTermVisitor() {

            @Override public boolean visit(IStrategoTerm term) {

                ImploderAttachment ia = term.getAttachment(ImploderAttachment.TYPE);
                if(excludeIdTerm && term.equals(ast)) {
                    return true;
                }
                if(ia.isNestedCompletion()) {
                    completionTerms.add(term);
                    return false;
                }
                return true;
            }
        };
        StrategoTermVisitee.topdown(visitor, ast);

        return completionTerms;
    }

    private StrategoTerm findTopMostAmbNode(StrategoTerm newNode) {
        StrategoTerm parent = (StrategoTerm) ParentAttachment.getParent(newNode);
        if(parent == null) {
            return newNode;
        }
        if(ImploderAttachment.getSort(parent) == null)
            return findTopMostAmbNode(parent);

        return newNode;
    }

    private StrategoTerm findTopMostCompletionNode(StrategoTerm newNode) {
        StrategoTerm parent = (StrategoTerm) ParentAttachment.getParent(newNode);
        if(parent == null) {
            return newNode;
        }

        ImploderAttachment ia = ImploderAttachment.get(parent);

        if(ia.getSort() == null || ia.isNestedCompletion()) {
            return newNode;
        }

        return findTopMostCompletionNode(parent);

    }

}
