package org.metaborg.spoofax.aesi.codecompletion;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import mb.nabl2.constraints.Constraints;
import mb.nabl2.constraints.IConstraint;
import mb.nabl2.constraints.ast.AstProperties;
import mb.nabl2.scopegraph.IScope;
import mb.nabl2.scopegraph.terms.Occurrence;
import mb.nabl2.scopegraph.terms.Scope;
import mb.nabl2.solver.messages.Messages;
import mb.nabl2.solver.solvers.ImmutableGraphSolution;
import mb.nabl2.solver.solvers.SingleFileSolver;
import mb.nabl2.spoofax.analysis.IResult;
import mb.nabl2.stratego.*;
import mb.nabl2.terms.ITerm;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.vfs2.FileObject;
import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.SourceToken;
import org.metaborg.core.MetaborgException;
import org.metaborg.core.context.IContext;
import org.metaborg.core.language.ILanguageComponent;
import org.metaborg.core.language.ILanguageImpl;
import org.metaborg.core.resource.IResourceService;
import org.metaborg.core.source.ISourceLocation;
import org.metaborg.core.source.ISourceRegion;
import org.metaborg.core.source.SourceLocation;
import org.metaborg.core.source.SourceRegion;
import org.metaborg.spoofax.core.context.constraint.IConstraintContext;
import org.metaborg.spoofax.core.stratego.IStrategoCommon;
import org.metaborg.spoofax.core.stratego.IStrategoRuntimeService;
import org.metaborg.spoofax.core.syntax.JSGLRSourceRegionFactory;
import org.metaborg.spoofax.core.syntax.SourceAttachment;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.tracing.ISpoofaxTracingService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.metaborg.util.functions.PartialFunction2;
import org.metaborg.util.task.NullCancel;
import org.metaborg.util.task.NullProgress;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spoofax.interpreter.terms.IStrategoAppl;
import org.spoofax.interpreter.terms.IStrategoList;
import org.spoofax.interpreter.terms.IStrategoTerm;
import org.spoofax.interpreter.terms.ITermFactory;
import org.spoofax.jsglr.client.imploder.IToken;
import org.spoofax.jsglr.client.imploder.ITokens;
import org.spoofax.jsglr.client.imploder.ImploderAttachment;
import org.spoofax.terms.visitor.AStrategoTermVisitor;
import org.spoofax.terms.visitor.IStrategoTermVisitor;
import org.spoofax.terms.visitor.StrategoTermVisitee;
import org.strategoxt.HybridInterpreter;
import mb.nabl2.solver.ISolution;

import javax.annotation.Nullable;
import java.util.*;

import static mb.nabl2.terms.build.TermBuild.B;

public final class NameCodeCompletionsProvider implements ICodeCompletionsProvider {

    private static final Logger logger = LoggerFactory.getLogger(NameCodeCompletionsProvider.class);
    private final IStrategoCommon strategoCommon;
    private final ITermFactoryService termFactoryService;
    private final IStrategoRuntimeService strategoRuntimeService;
    private final IResourceService resourceService;
    private final ISpoofaxTracingService tracingService;

    /**
     * Initializes a new instance of the {@link NameCodeCompletionsProvider} class.
     *
     * @param strategoCommon Helper for invoking Stratego strategies.
     * @param termFactoryService
     * @param strategoRuntimeService
     * @param resourceService
     */
    @Inject
    public NameCodeCompletionsProvider(
            IStrategoCommon strategoCommon,
            ITermFactoryService termFactoryService,
            IStrategoRuntimeService strategoRuntimeService,
            IResourceService resourceService,
            ISpoofaxTracingService tracingService
    ) {
        assert strategoCommon != null;
        assert termFactoryService != null;
        assert strategoRuntimeService != null;
        assert resourceService != null;
        assert tracingService != null;

        this.strategoCommon = strategoCommon;
        this.termFactoryService = termFactoryService;
        this.strategoRuntimeService = strategoRuntimeService;
        this.resourceService = resourceService;
        this.tracingService = tracingService;
    }

    @Override
    public List<SpoofaxCodeCompletionProposal> getCompletions(
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            SourceToken prefix) {
        if (parseResult == null) return Collections.emptyList();
        if (analysisResult == null) return Collections.emptyList();

        List<SpoofaxCodeCompletionProposal> proposals = new LinkedList<>();

        final FileObject resource = parseResult.source();
        final ILanguageImpl language = parseResult.input().langImpl();
        final String languageName = language.belongsTo().name();

        for(ILanguageComponent component : language.components()) {
            HybridInterpreter runtime;
            try {
                runtime = strategoRuntimeService.runtime(component, resource, false);
            } catch (MetaborgException e) {
                throw new RuntimeException(e);
            }

            ITermFactory termFactory = termFactoryService.get(component, null, false);
            Map<IStrategoTerm, Boolean> leftRecursiveTerms = new HashMap<>();
            Map<IStrategoTerm, Boolean> rightRecursiveTerms = new HashMap<>();
            IStrategoTerm ast = parseResult.ast();
            List<IStrategoTerm> terms = findAstTermsAtOffset(ast, caretOffset, runtime, termFactory, leftRecursiveTerms, rightRecursiveTerms);

            @Nullable IStrategoAppl placeholder = getPlaceholderFromTerms(terms, caretOffset);
            // TODO: This is only for debugging
            proposals.add(new SpoofaxCodeCompletionProposal(placeholder != null ? "Plhdr: " + placeholder.toString() : "NO PLACEHOLDER", null, null, null, null, null));
            for (IStrategoTerm t : terms) {
                proposals.add(new SpoofaxCodeCompletionProposal(t.toString(), null, null, null, null, null));
//                proposals.add(new SpoofaxCodeCompletionProposal(t.toString(), null, null, null, new ScopeNames("expansion.editing.spoofax"), null));
            }
            // --------- //

            if (placeholder != null) {
                logger.debug("Completing on placeholder: " + placeholder.toString());

                final IContext context = analysisResult.context();

                TermIndex termIndex = StrategoTermIndices.get(placeholder).get();
                if (context instanceof IConstraintContext) {
                    IConstraintContext ccontext = (IConstraintContext)context;
                    String termResource = termIndex.getResource();
                    IStrategoTerm analysisTerm = ccontext.getAnalysis(termResource);
                    Optional<IResult> optResult = StrategoBlob.match(analysisTerm, IResult.class);
                    if (optResult.isPresent()) {
                        IResult result = optResult.get();
                        ITerm params = result.solution().astProperties().getValue(termIndex, AstProperties.PARAMS_KEY).get();
                        ITerm type = result.solution().astProperties().getValue(termIndex, AstProperties.TYPE_KEY).get();

                        // For each scope
                        IScope s;
                        // find the occurrences visible from that scope
                        Set<Occurrence> occurrences = result.solution().nameResolution().visible(s).get();

                        // For each pair of occurrence x syntactic completion
                        // we create a new template with the occurrence name as the first $ID.
                        Occurrence o;
                        ITerm name = o.getName();
                        StrategoTerms s;
                        IStrategoTerm strategoName = s.toStratego(name);

                        // then a miracle happens

                        // Then we add a constraint to the solution

                        // First call Stratego to create the constraint:
                        // args = Params(params) (in het geval van NoType()) of Params(params, type)
                        // constraintTerm =  nabl2--generate-constraint-completion Tuple(strategoName, fragment, args)
                        IStrategoTerm constraintTerm = null;
                        IConstraint constraint = ConstraintTerms.specialize(Constraints.matcher()).match(s.fromStratego(constraintTerm)).get();

                        // Add the constraint to the solution
                        // We remove the messages from the solution so that we can see any errors resulting from adding the constraint.
                        CompletionSolver solver = new CompletionSolver(PartialFunction2.never());

                        ISolution oldSolution = result.solution();
                        oldSolution.withConstraints(completionConstraints).withMessages(Messages.Immutable.of());
                        fresh = IResult.fresh().melt()::fresh;
                        ISolution newSolution = solver.solve(currentSolution, fresh, new NullCancel(), new NullProgress());

                        // If the solution does not contain any errors,
                        // the fragment is semantically and syntactically valid and can be suggested as a completion
                        newSolution.messages().getErrors().isEmpty();
                    }
                }

                // ------ //

                final Iterable<IStrategoTerm> inRegion = tracingService.fragments(analysisResult, new SourceRegion(caretOffset));
                logger.debug("InRegion: " + StringUtils.join(inRegion, "; "));
            } else {
                logger.debug("Completion not in placeholder.");
                final Iterable<IStrategoTerm> optionals = getOptionalsFromTerms(terms, leftRecursiveTerms, rightRecursiveTerms);
                final Iterable<IStrategoTerm> leftRecursive = getLeftRecursiveTermsFromTerms(terms, caretOffset, leftRecursiveTerms);
                final Iterable<IStrategoTerm> rightRecursive = getRightRecursiveTermsFromTerms(terms, caretOffset, rightRecursiveTerms);
                // TODO
            }
        }
        return proposals;
    }

    private Optional<? extends ITerm> getNamesVisibleInScope(ITerm term, ISolution solution) {
        return Scope.matcher().match(term, solution.unifier()).<ITerm>flatMap(
                scope -> solution.nameResolution().visible(scope).map(B::newList)
        );
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
     * Includes any lauyout from the specified token in the specified direction.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token.
     */
    private IToken includeLayout(ITokens tokenizer, IToken token, int direction) {
        IToken newToken = token;
        for(int i = token.getIndex() + direction; i >= 0 && i < tokenizer.getTokenCount(); i += direction) {
            if (tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF ||
                    tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                newToken = tokenizer.getTokenAt(i);
            } else {
                break;
            }
        }
        return newToken;
    }

    /**
     * Excludes any layout from the specified token in the specified direction up to the specified end token.
     *
     * @param tokenizer The tokenizer.
     * @param token The token where to start.
     * @param end The token where to end.
     * @param direction The direction, either -1 for left or +1 for right.
     * @return The new token.
     */
    private IToken excludeLayout(ITokens tokenizer, IToken token, IToken end, int direction) {
        IToken newToken = token;
        for(int i = token.getIndex(); i != end.getIndex(); i += direction) {
            if (tokenizer.getTokenAt(i).getKind() == IToken.TK_LAYOUT ||
                tokenizer.getTokenAt(i).getKind() == IToken.TK_EOF ||
                tokenizer.getTokenAt(i).getKind() == IToken.TK_ERROR) {
                newToken = tokenizer.getTokenAt(i + direction);
            } else {
                break;
            }
        }
        return newToken;
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
        for(IStrategoTerm term : terms) {
            if(term instanceof IStrategoAppl) {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);

                // A placeholder is a term whose constructor ends with "-Plhdr".
                final IStrategoAppl appl = (IStrategoAppl) term;
                if (appl.getConstructor().getName().endsWith("-Plhdr") &&
                        caretOffset > left.getStartOffset() &&
                        caretOffset <= right.getEndOffset()) {
                    return appl;
                }
            }
        }

        return null;
    }

    /**
     * Gets the list terms from the list of terms.
     *
     * This returns the terms in the same order as the input list of terms.
     *
     * @param terms The list of terms.
     * @param leftRecursiveTerms The left-recursive terms.
     * @param rightRecursiveTerms The right-recursive terms.
     * @return The list terms.
     */
    private Iterable<IStrategoList> getListsFromTerms(
            final Iterable<IStrategoTerm> terms,
            Map<IStrategoTerm, Boolean> leftRecursiveTerms,
            Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoList> lists = Lists.newLinkedList();
        for (IStrategoTerm term : terms) {
            if (term instanceof IStrategoList) {
                final IStrategoList list = (IStrategoList) term;
                lists.add(list);
            } else {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);
                // if term is not nullable, nor a list nor left or right recursive stop the search
                if (left.getStartOffset() <= right.getEndOffset()) {
                    boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);
                    boolean isRightRecursive = rightRecursiveTerms.containsKey(term);
                    if (!isLeftRecursive && !isRightRecursive) {
                        break;
                    }
                }
            }
        }

        return lists;
    }

    /**
     * Gets the optional terms from the list of terms.
     *
     * This returns the terms in the same order as the input list of terms.
     *
     * @param terms The list of terms.
     * @param leftRecursiveTerms The left-recursive terms.
     * @param rightRecursiveTerms The right-recursive terms.
     * @return The optional terms.
     */
    private Iterable<IStrategoTerm> getOptionalsFromTerms(
            final Iterable<IStrategoTerm> terms,
            Map<IStrategoTerm, Boolean> leftRecursiveTerms,
            Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoTerm> optionals = Lists.newLinkedList();
        for (IStrategoTerm term : terms) {
            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);
            if (!(term instanceof IStrategoList) && left.getStartOffset() > right.getEndOffset()) {
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

    /**
     * Gets the left-recursive terms from the list of terms.
     *
     * This returns the terms in the same order as the input list of terms.
     *
     * @param terms The list of terms.
     * @param caretOffset The caret offset.
     * @param leftRecursiveTerms The left-recursive terms.
     * @return The optional terms.
     */
    private Iterable<IStrategoTerm> getLeftRecursiveTermsFromTerms(
            Iterable<IStrategoTerm> terms,
            int caretOffset,
            Map<IStrategoTerm, Boolean> leftRecursiveTerms) {
        Collection<IStrategoTerm> leftRecursive = Lists.newLinkedList();
        for (IStrategoTerm term : terms) {
            boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);

            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);

            if (isLeftRecursive && caretOffset > right.getEndOffset()) {
                leftRecursive.add(term);
            } else if (term instanceof IStrategoList || left.getStartOffset() > right.getEndOffset()) {
                continue;
            } else {
                break;
            }
        }

        return leftRecursive;
    }

    /**
     * Gets the right-recursive terms from the list of terms.
     *
     * This returns the terms in the same order as the input list of terms.
     *
     * @param terms The list of terms.
     * @param caretOffset The caret offset.
     * @param rightRecursiveTerms The right-recursive terms.
     * @return The optional terms.
     */
    private Iterable<IStrategoTerm> getRightRecursiveTermsFromTerms(
            Iterable<IStrategoTerm> terms,
            int caretOffset,
            Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

        Collection<IStrategoTerm> rightRecursive = Lists.newLinkedList();
        for (IStrategoTerm term : terms) {
            boolean isRightRecursive = rightRecursiveTerms.containsKey(term);

            IToken left = ImploderAttachment.getLeftToken(term);
            IToken right = ImploderAttachment.getRightToken(term);

            if (isRightRecursive && caretOffset <= left.getStartOffset()) {
                rightRecursive.add(term);
            } else if (term instanceof IStrategoList || left.getStartOffset() > right.getEndOffset()) {
                continue;
            } else {
                break;
            }
        }

        return rightRecursive;
    }
}
