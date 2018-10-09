package org.metaborg.spoofax.aesi.codecompletion;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import mb.nabl2.constraints.ast.AstProperties;
import mb.nabl2.scopegraph.terms.Occurrence;
import mb.nabl2.scopegraph.terms.Scope;
import mb.nabl2.spoofax.analysis.IResult;
import mb.nabl2.stratego.TermIndex;
import mb.nabl2.terms.ITerm;
import mb.nabl2.terms.matching.Transform;
import mb.nabl2.terms.unification.IUnifier;
import org.apache.commons.vfs2.FileObject;
import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.SourceToken;
import org.metaborg.core.MetaborgException;
import org.metaborg.core.completion.ICompletion;
import org.metaborg.core.context.IContext;
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
import org.metaborg.spoofax.core.syntax.*;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.tracing.ISpoofaxTracingService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxInputUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxUnitService;
import org.metaborg.util.functions.Function1;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spoofax.interpreter.core.Tools;
import org.spoofax.interpreter.terms.*;
import org.spoofax.jsglr.client.imploder.IToken;
import org.spoofax.jsglr.client.imploder.ITokens;
import org.spoofax.jsglr.client.imploder.ImploderAttachment;
import org.spoofax.jsglr.client.imploder.ListImploderAttachment;
import org.spoofax.terms.StrategoAppl;
import org.spoofax.terms.attachments.ParentAttachment;
import org.spoofax.terms.visitor.AStrategoTermVisitor;
import org.spoofax.terms.visitor.IStrategoTermVisitor;
import org.spoofax.terms.visitor.StrategoTermVisitee;
import org.strategoxt.HybridInterpreter;

import javax.annotation.Nullable;
import java.util.*;

/**
 * Provides syntactic code completion.
 */
public class SyntaxCodeCompletionsProvider implements ICodeCompletionsProvider {

    private static final String PLACEHOLDER_SORT_SUFFIX = "-Plhdr";

    private static final Logger logger = LoggerFactory.getLogger(SpoofaxCodeCompletionsProvider.class);
    protected final IStrategoCommon strategoCommon;
    protected final ITermFactoryService termFactoryService;
    protected final IStrategoRuntimeService strategoRuntimeService;
    protected final IResourceService resourceService;
    protected final ISpoofaxTracingService tracingService;
    protected final ISpoofaxUnitService unitService;
    protected final ISpoofaxSyntaxService syntaxService;

    /**
     * Initializes a new instance of the {@link SyntaxCodeCompletionsProvider} class.
     */
    @Inject
    protected SyntaxCodeCompletionsProvider(
            IStrategoCommon strategoCommon,
            ITermFactoryService termFactoryService,
            IStrategoRuntimeService strategoRuntimeService,
            IResourceService resourceService,
            ISpoofaxTracingService tracingService,
            ISpoofaxUnitService unitService,
            ISpoofaxSyntaxService syntaxService
    ) {
        assert strategoCommon != null;
        assert termFactoryService != null;
        assert strategoRuntimeService != null;
        assert resourceService != null;
        assert tracingService != null;
        assert unitService != null;
        assert syntaxService != null;

        this.strategoCommon = strategoCommon;
        this.termFactoryService = termFactoryService;
        this.strategoRuntimeService = strategoRuntimeService;
        this.resourceService = resourceService;
        this.tracingService = tracingService;
        this.unitService = unitService;
        this.syntaxService = syntaxService;
    }

    @Override
    public List<SpoofaxCodeCompletionProposal> getCompletions(
            ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            SourceToken prefix) {

        List<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

        final ISpoofaxInputUnit input = parseResult.input();
        final ILanguageImpl language = input.langImpl();
        final FileObject location = parseResult.source();
        final String text = input.text();

        if (text.trim().isEmpty()) {
            List<SpoofaxCodeCompletionProposal> startProposals = getCompletionProposalsForEmptyDocument(
                    language, location, text
            );
            proposals.addAll(startProposals);
        }

        boolean nested = false; // TODO: Nested?
        if (parseResult.success()) {
            List<SpoofaxCodeCompletionProposal> proposals1 = getCompletionProposalsForCorrectDocument(language, location, caretOffset, parseResult);
            proposals.addAll(proposals1);
        } else if (!nested) {
            // When parsing failed, and we are not in a nested scope,
            // reparse, with recovery and completions enabled.
            ISpoofaxParseUnit recoveredParseResult = reparseWithRecoveryAndCompletions(parseResult, caretOffset);

            if (recoveredParseResult == null || recoveredParseResult.ast() == null)
                return Collections.emptyList();

            // TODO:
//            Collection<IStrategoTerm> nestedCompletionTerms = getNestedCompletionTermsFromAST(completionParseResult);
//            if(!nestedCompletionTerms.isEmpty()) {
//                proposals.addAll(completionErroneousProgramsNested(position, nestedCompletionTerms, completionParseResult));
//            }
//
//            Collection<IStrategoTerm> completionTerms = getCompletionTermsFromAST(completionParseResult);
//            if(!completionTerms.isEmpty()) {
//                proposals.addAll(completionErroneousPrograms(position, completionTerms, completionParseResult));
//            }
        }

        return proposals;
    }

    /**
     * Reparses the input with recovery and completions enabled.
     *
     * @param parseResult The initial parse result.
     * @param caretOffset The caret offset.
     * @return The new parse result.
     */
    private ISpoofaxParseUnit reparseWithRecoveryAndCompletions(ISpoofaxParseUnit parseResult, int caretOffset) {
        final JSGLRParserConfiguration config = new JSGLRParserConfiguration(
                true, true, true, 3000, caretOffset);
        final ISpoofaxInputUnit input = parseResult.input();
        final ISpoofaxInputUnit modifiedInput =
                this.unitService.inputUnit(input.source(), input.text(), input.langImpl(), input.dialect(), config);
        try {
            return this.syntaxService.parse(modifiedInput);
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Determines the completion proposals for an empty document.
     *
     * @param language The language of the document.
     * @param location The document location.
     * @param text The document text.
     * @return The proposals; or an empty list.
     */
    private List<SpoofaxCodeCompletionProposal> getCompletionProposalsForEmptyDocument(
            ILanguageImpl language, FileObject location, String text) {
        @Nullable final SyntaxFacet facet = language.facet(SyntaxFacet.class);
        if (facet == null) return Collections.emptyList();

        List<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

        for(ILanguageComponent component : language.components()) {
            List<SpoofaxCodeCompletionProposal> emptyDocumentProposals = createCompleter(component, language, location)
                    .getEmptyDocumentProposals();
            proposals.addAll(emptyDocumentProposals);
        }

        return proposals;
    }

    /**
     * Determines the completion proposals for a correct document.
     *
     * @param language The language of the document.
     * @param location The document location.
     * @param position The caret offset in the document.
     * @param parseResult The parse result.
     * @return The proposals; or an empty list.
     */
    public List<SpoofaxCodeCompletionProposal> getCompletionProposalsForCorrectDocument(
            ILanguageImpl language, FileObject location,
            int position, ISpoofaxParseUnit parseResult) {

        List<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();
        boolean blankLineCompletion = isWhitespaceLine(parseResult.input().text(), position);

        for(ILanguageComponent component : language.components()) {

            List<SpoofaxCodeCompletionProposal> correctDocumentProposals = createCompleter(component, language, location)
                    .getCorrectDocumentProposals(parseResult.ast(), position, blankLineCompletion);
            proposals.addAll(correctDocumentProposals);


        }
        return proposals;
    }

    /**
     * Determines whether the character with the specified offset is on a line that consists
     * of only whitespace.
     *
     * @param text The text to analyze.
     * @param offset The zero-based character offset in the text.
     * @return True when the offset is on an empty or whitespace line; otherwise, false.
     */
    private static boolean isWhitespaceLine(String text, int offset) {

        // Move back across whitespace until we find a newline or the start of the text.
        for (int i = offset - 1; i >= 0 && text.charAt(i) != '\n'; i--) {
            if (!Character.isWhitespace(text.charAt(i))) {
                return false;
            }
        }

        // Move forward across whitespace until we find a newline or the end of the text.
        for (int i = offset; i < text.length() && text.charAt(i) != '\n'; i++) {
            if (!Character.isWhitespace(text.charAt(i))) {
                return false;
            }
        }

        // We have found the start of the line, the end of the line,
        // and seen only whitespace in between.
        return true;
    }

    /**
     * Creates a new completer object.
     * @param languageComponent The language component.
     * @param language The language.
     * @param location The document location.
     * @return The completer.
     */
    private Completer createCompleter(ILanguageComponent languageComponent, ILanguageImpl language, FileObject location) {
        final HybridInterpreter runtime;
        try {
            runtime = strategoRuntimeService.runtime(languageComponent, location, false);
        } catch (MetaborgException e) {
            throw new RuntimeException(e);
        }
        final ITermFactory termFactory = termFactoryService.get(languageComponent, null, false);

        return new Completer(this.strategoCommon, runtime, termFactory, languageComponent, language, this.resourceService);
    }

    /**
     * Helps with the completions.
     */
    private static class Completer {
        final IStrategoCommon strategoCommon;
        final HybridInterpreter runtime;
        final ITermFactory termFactory;
        final ILanguageComponent languageComponent;
        final ILanguageImpl language;
        final IResourceService resourceService;

        Completer(
                IStrategoCommon strategoCommon,
                HybridInterpreter runtime,
                ITermFactory termFactory,
                ILanguageComponent languageComponent,
                ILanguageImpl language,
                IResourceService resourceService) {
            this.strategoCommon = strategoCommon;
            this.runtime = runtime;
            this.termFactory = termFactory;
            this.languageComponent = languageComponent;
            this.language = language;
            this.resourceService = resourceService;
        }

        /**
         * Gets the language name.
         *
         * @return The language name.
         */
        private String getLanguageName() {
            return this.language.belongsTo().name();
        }

        /**
         * Gets the code completion proposals for an empty document.
         *
         * @return The completion proposals.
         */
        private List<SpoofaxCodeCompletionProposal> getEmptyDocumentProposals() {
            List<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

            final Iterable<String> startSymbols = getStartSymbols(this.language);
            for(String startSymbol : startSymbols) {
                String placeholderName = getPlaceholderSortName(startSymbol);
                IStrategoAppl placeholder = this.termFactory.makeAppl(this.termFactory.makeConstructor(placeholderName, 0));
                IStrategoTuple input = this.termFactory.makeTuple(this.termFactory.makeString(startSymbol), placeholder);

                @Nullable final IStrategoTerm proposalsPlaceholder = invoke("get-proposals-empty-program-" + getLanguageName(), input);

                if (proposalsPlaceholder == null) {
                    logger.error("Getting proposals for {} failed", placeholder);
                    continue;
                }

                for (IStrategoTerm proposalTerm : proposalsPlaceholder) {
                    if (!(proposalTerm instanceof IStrategoTuple)) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                    if (tuple.getSubtermCount() != 2 || !(tuple.getSubterm(0) instanceof IStrategoString)
                            || !(tuple.getSubterm(1) instanceof IStrategoString)) {
                        logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    final String name = Tools.asJavaString(tuple.getSubterm(0));
                    final String text = Tools.asJavaString(tuple.getSubterm(1));

                    proposals.add(new JsglrCodeCompletionProposal(
                            text,
                            startSymbol,
                            name,
                            null,
                            "",
                            new ScopeNames("expansion.spoofax"),
                            null
                    ));
                }
            }

            return proposals;
        }

        /**
         * Gets the code completion proposals for a correct document.
         *
         * A correct document is a document that was completely parsed without errors,
         * but it may contain placeholders.
         *
         * @return The completion proposals.
         */
        /* package privbate */ List<SpoofaxCodeCompletionProposal> getCorrectDocumentProposals(
                IStrategoTerm ast,
                int caretOffset,
                boolean blankLineCompletion) {
            List<SpoofaxCodeCompletionProposal> proposals = new LinkedList<>();
            Map<IStrategoTerm, Boolean> leftRecursiveTerms = new HashMap<>();
            Map<IStrategoTerm, Boolean> rightRecursiveTerms = new HashMap<>();
//            IStrategoTerm ast = analysisResult.ast();
            List<IStrategoTerm> terms = findAstTermsAtOffset(ast, caretOffset, runtime, termFactory, leftRecursiveTerms, rightRecursiveTerms);

            @Nullable IStrategoAppl placeholder = getPlaceholderFromTerms(terms, caretOffset);

//            final IContext context = analysisResult.context();
            if (placeholder != null) {// && context instanceof IConstraintContext) {
//                // From the placeholder, the most specific element:
//                logger.debug("Completing on placeholder: " + placeholder.toString());
//                IConstraintContext ccontext = (IConstraintContext)context;
//
//                @Nullable TermIndex termIndex = getTermIndex(placeholder);
//                if (termIndex == null) return Collections.emptyList();
//                @Nullable IResult result = getAnalysisResult(termIndex, ccontext);
//                if (result == null) return Collections.emptyList();
//
//                Function1<String, String> fresh = result.fresh().melt()::fresh;
//
//                // Get the term's [[ _ ^ args : type ]].
//                @Nullable ITerm args = result.solution().astProperties().getValue(termIndex, AstProperties.PARAMS_KEY).orElse(null);
//                @Nullable ITerm type = result.solution().astProperties().getValue(termIndex, AstProperties.TYPE_KEY).orElse(null);
//                if (args == null || type == null) return Collections.emptyList();
//
//                // Get the scopes from the args.
//                IUnifier unifier = result.solution().unifier();
//                Collection<Scope> scopes = Transform.T.collecttd(t -> Scope.matcher().match(t, unifier)).apply(args);
//
//                Set<Occurrence> occurrences = getOccurrencesInScopes(scopes, result.solution());
//                for (IStrategoTerm syntaxFragment : terms) {
//                    List<SpoofaxCodeCompletionProposal> fragmentProposals = findSemanticCompletionsForFragment(syntaxFragment, args, type, fresh, result, occurrences, strategoTerms, solver);
//                    proposals.addAll(fragmentProposals);
//                }
                proposals.addAll(placeholderCompletions(placeholder, caretOffset));
            } else {
                logger.debug("Completion not in placeholder.");
                final Collection<IStrategoList> lists = getListsFromTerms(terms, leftRecursiveTerms, rightRecursiveTerms);
                if (!lists.isEmpty()) {
                    proposals.addAll(listsCompletions(caretOffset, blankLineCompletion, lists));
                }

                final Collection<IStrategoTerm> optionals = getOptionalsFromTerms(terms, leftRecursiveTerms, rightRecursiveTerms);
                if (!optionals.isEmpty()) {
                    proposals.addAll(optionalCompletions(caretOffset, blankLineCompletion, optionals));
                }

                // TODO Improve recursive completions
//                final Collection<IStrategoTerm> leftRecursive = getLeftRecursiveTermsFromTerms(terms, caretOffset, leftRecursiveTerms);
//                final Collection<IStrategoTerm> rightRecursive = getRightRecursiveTermsFromTerms(terms, caretOffset, rightRecursiveTerms);
//                if (!leftRecursive.isEmpty() || !rightRecursive.isEmpty()) {
//                    proposals
//                            .addAll(recursiveCompletions(leftRecursive, rightRecursive, languageName, component, location));
//                }
            }

            return proposals;
        }

        /**
         * Gets the start symbols for the specified language.
         *
         * @param language The language.=
         * @return The start symbols.
         */
        private static Iterable<String> getStartSymbols(ILanguageImpl language) {
            @Nullable final SyntaxFacet facet = language.facet(SyntaxFacet.class);
            if (facet == null) return Collections.emptyList();
            return facet.startSymbols;
        }

        /**
         * Invokes a Stratego strategy with the specified arguments.
         *
         * @param strategy The name of the strategy.
         * @param input The input term to the strategy.
         * @return The result; or null when the strategy failed.
         */
        @Nullable
        private IStrategoTerm invoke(String strategy, IStrategoTerm input) {
            try {
                return this.strategoCommon.invoke(this.runtime, input, strategy);
            } catch (MetaborgException e) {
                return null;
            }
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
        private Collection<IStrategoList> getListsFromTerms(
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
        private Collection<IStrategoTerm> getOptionalsFromTerms(
                final Iterable<IStrategoTerm> terms,
                Map<IStrategoTerm, Boolean> leftRecursiveTerms,
                Map<IStrategoTerm, Boolean> rightRecursiveTerms) {

            Collection<IStrategoTerm> optionals = Lists.newLinkedList();
            for (IStrategoTerm term : terms) {
                IToken left = ImploderAttachment.getLeftToken(term);
                IToken right = ImploderAttachment.getRightToken(term);
                if (term instanceof IStrategoList) {
                    // The term is a list, which we ignore.
                    continue;
                } else if (left.getStartOffset() > right.getEndOffset()) {
                    // The term is nullable.
                    optionals.add(term);
                } else {
                    boolean isLeftRecursive = leftRecursiveTerms.containsKey(term);
                    boolean isRightRecursive = rightRecursiveTerms.containsKey(term);
                    if (!isLeftRecursive && !isRightRecursive) {
                        // The term is neither a list, nor nullable, not left or right recursive.
                        // Stop the search.
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
        private Collection<IStrategoTerm> getLeftRecursiveTermsFromTerms(
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
        private Collection<IStrategoTerm> getRightRecursiveTermsFromTerms(
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

        /**
         * Calls a Stratego strategy to get proposals for completing a placeholder.
         *
         * @param placeholder The placeholder.
         * @return The proposals.
         */
        private Collection<SpoofaxCodeCompletionProposal> placeholderCompletions(IStrategoAppl placeholder, int caretOffset) {
            // Stratego term building and deconstruction

            final IStrategoTerm placeholderParent = getPlaceholderOrParent(placeholder);
            final IStrategoInt placeholderIdx = this.termFactory.makeInt(getPlaceholderIndex(placeholderParent, placeholder));
            final String placeholderSort = ImploderAttachment.getSort(placeholder);
            final IStrategoTerm strategoInput =
                    this.termFactory.makeTuple(this.termFactory.makeString(placeholderSort), placeholder, placeholderParent, placeholderIdx);

            final IStrategoTerm proposalTerms = invoke("get-proposals-placeholder-" + getLanguageName(), strategoInput);
            if (proposalTerms == null) {
                logger.error("Getting proposals for {} failed", placeholder);
                return Collections.emptyList();
            }

            return getProposals(proposalTerms, false, caretOffset);
        }

        /**
         * Constructs code completion proposals from the given list of Stratego proposal terms,
         * each in the form of a tuple (name: String, text: String, additionalInfo: String, change: Change)
         * where Change is either REPLACE_TERM(oldNode, newNode), INSERT_AT_END(..), or INSERT_BEFORE(..).
         *
         * @param proposalTerms The proposal terms.
         * @return The constructed code completion proposals.
         */
        private Collection<SpoofaxCodeCompletionProposal> getProposals(IStrategoTerm proposalTerms, boolean blankLineCompletion, int caretOffset) {
            Collection<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

            for (IStrategoTerm proposalTerm : proposalTerms) {
                if (!(proposalTerm instanceof IStrategoTuple)) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }

                final IStrategoTuple tuple = (IStrategoTuple) proposalTerm;
                if ((tuple.getSubtermCount() < 4)
                        || !(tuple.getSubterm(0) instanceof IStrategoString)
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

                String prefix;
                String suffix;
                if (tuple.getSubtermCount() == 6) {
                    if (tuple.getSubterm(4) == null
                            || !(tuple.getSubterm(5) instanceof IStrategoString)) {
                        logger.error("Unexpected extended proposal term {}, skipping", proposalTerm);
                        continue;
                    }

                    final IStrategoTerm completionTerm = tuple.getSubterm(4);
                    final String completionKind = Tools.asJavaString(tuple.getSubterm(5));
                    prefix = calculatePrefix(caretOffset, completionTerm);
                    suffix = calculateSuffix(caretOffset, completionTerm);
                } else {
                    prefix = "";
                    suffix = "";
                }

                final SpoofaxCodeCompletionProposal proposal;
                if (change.getConstructor().getName().contains("REPLACE_TERM")) {
                    proposal = createCompletionReplaceTerm(name, text, additionalInfo, change, false, "", "");
                } else if (change.getConstructor().getName().contains("INSERT_AT_END")) {
                    // if the change is inserting at the end of a list
                    proposal = createCompletionInsertAtEnd(name, text, additionalInfo, change, blankLineCompletion);
                } else if (change.getConstructor().getName().contains("INSERT_BEFORE")) {
                    proposal = createCompletionInsertBefore(name, text, additionalInfo, change);
                } else {
                    proposal = null;
                }

                if (proposal == null) {
                    logger.error("Unexpected proposal term {}, skipping", proposalTerm);
                    continue;
                }

                proposals.add(proposal);
            }

            return proposals;
        }

        private String calculatePrefix(int caretOffset, IStrategoTerm proposalTerm) {

            String prefix = "";
            ITokens tokenizer = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken().getTokenizer();
            IToken leftToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken();
            IToken rightToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getRightToken();

            IToken current = leftToken;
            int endOffsetPrefix = Integer.MIN_VALUE;
            while (current.getEndOffset() < caretOffset && current != rightToken) {
                if (endOffsetPrefix < current.getEndOffset()) {
                    prefix = prefix + current.toString();
                    endOffsetPrefix = current.getEndOffset();
                }
                current = tokenizer.getTokenAt(current.getIndex() + 1);
            }

            return prefix;
        }


        private String calculateSuffix(int caretOffset, IStrategoTerm proposalTerm) {

            String suffix = "";
            ITokens tokenizer = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken().getTokenizer();
            IToken leftToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getLeftToken();
            IToken rightToken = proposalTerm.getAttachment(ImploderAttachment.TYPE).getRightToken();

            IToken current = rightToken;
            int startOffsetSuffix = Integer.MAX_VALUE;
            while (current.getStartOffset() >= caretOffset && current != leftToken) {
                if (startOffsetSuffix > current.getStartOffset()) {
                    suffix = current.toString() + suffix;
                    startOffsetSuffix = current.getStartOffset();
                }
                current = tokenizer.getTokenAt(current.getIndex() - 1);
            }

            return suffix;
        }

        /**
         * Calls a Stratego strategy to get proposals for completing a list.
         */
        private Collection<SpoofaxCodeCompletionProposal> listsCompletions(
                int caretOffset,
                boolean blankLineCompletion,
                Iterable<IStrategoList> lists) {

            Collection<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

            for (IStrategoList list : lists) {
                ListImploderAttachment attachment = list.getAttachment(null);
                String sort = attachment.getSort().substring(0, attachment.getSort().length() - 1);
                String placeholderName = getPlaceholderSortName(sort);
                IStrategoAppl listPlaceholder = this.termFactory.makeAppl(this.termFactory.makeConstructor(placeholderName, 0));
                final IStrategoTerm strategoInput = this.termFactory.makeTuple(this.termFactory.makeString(sort), list,
                        listPlaceholder, termFactory.makeInt(caretOffset));

                final IStrategoTerm proposalsLists = invoke("get-proposals-list-" + getLanguageName(), strategoInput);
                if (proposalsLists == null) {
                    logger.error("Getting proposals for {} failed", strategoInput);
                    return Collections.emptyList();
                }

                proposals.addAll(getProposals(proposalsLists, blankLineCompletion, caretOffset));
            }

            return proposals;
        }

        /**
         * Calls a Stratego strategy to get proposals for completing an optional term.
         */
        private Collection<SpoofaxCodeCompletionProposal> optionalCompletions(
                int caretOffset,
                boolean blankLineCompletion,
                Iterable<IStrategoTerm> optionals) {

            Collection<SpoofaxCodeCompletionProposal> proposals = Lists.newLinkedList();

            for (IStrategoTerm optional : optionals) {

                ImploderAttachment attachment = optional.getAttachment(ImploderAttachment.TYPE);
                String sort = attachment.getSort();
                String placeholderName = getPlaceholderSortName(sort);
                IStrategoAppl optionalPlaceholder = termFactory.makeAppl(termFactory.makeConstructor(placeholderName, 0));
                final IStrategoTerm strategoInput =
                        termFactory.makeTuple(termFactory.makeString(sort), optional, optionalPlaceholder);

                // call Stratego part of the framework to compute change
                final IStrategoTerm proposalsOptional = invoke("get-proposals-optional-" + getLanguageName(), strategoInput);

                if (proposalsOptional == null) {
                    logger.error("Getting proposals for {} failed", strategoInput);
                    continue;
                }

                proposals.addAll(getProposals(proposalsOptional, blankLineCompletion, caretOffset));
            }

            return proposals;
        }

        /**
         * Creates a completion proposal that replaces a term.
         */
        private JsglrCodeCompletionProposal createCompletionReplaceTerm(
                String name,
                String text,
                String additionalInfo,
                StrategoAppl change,
                boolean blankLineCompletion,
                String prefix,
                String suffix) {

            final IStrategoTerm oldNode = change.getSubterm(0);
            final IStrategoTerm newNode = change.getSubterm(1);

            if (change.getSubtermCount() != 2
                    || !(newNode instanceof IStrategoAppl)
                    || !(oldNode instanceof IStrategoAppl)) {
                return null;
            }

            final String sort = ImploderAttachment.getSort(oldNode);

            int insertionPoint, suffixPoint;

            final ImploderAttachment oldNodeIA = oldNode.getAttachment(ImploderAttachment.TYPE);
            ITokens tokenizer = ImploderAttachment.getTokenizer(oldNode);
            IToken leftToken = oldNodeIA.getLeftToken();
            IToken rightToken = oldNodeIA.getRightToken();

            // check if it's an empty node
            boolean isEmptyNode = leftToken.getStartOffset() > rightToken.getEndOffset();
            if (isEmptyNode) {
                // get the last non-layout token before the new node
                int tokenPosition = includeLayoutByIndex(tokenizer, leftToken, -1);
                insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();

                // TODO: This code is very dense.
                // TODO: Can we remove the dependency on 'additionalInfo'?
                // if completion does not span multiple lines preserve everything starting at the first non-layout char
                if (!additionalInfo.contains("\n")) {
                    suffixPoint = includeLayout(tokenizer, leftToken, +1).getStartOffset();
                } else { // if completion spams multiple lines keep the lines
                    suffixPoint = insertionPoint + 1;
                }
                // if completion is triggered in an empty line, consume that line
                IToken checkToken;
                if (blankLineCompletion) {
                    for (; tokenPosition < tokenizer.getTokenCount(); tokenPosition++) {
                        checkToken = tokenizer.getTokenAt(tokenPosition);
                        if (tokenizer.toString(checkToken, checkToken).contains("\n")) {
                            suffixPoint = checkToken.getEndOffset();
                        }
                    }
                }


            } else { // if not, do a regular replacement
                insertionPoint = leftToken.getStartOffset() - 1;
                suffixPoint = rightToken.getEndOffset() + 1;
            }

            ScopeNames scopeNames;
            if (prefix.equals("") && suffix.equals("")) {
                scopeNames = new ScopeNames(SpoofaxScopeNames.EXPANSION);
            } else {
                scopeNames = new ScopeNames(SpoofaxScopeNames.EXPANSION_EDITING);
            }

            return createProposal(text, sort, name, insertionPoint, suffixPoint, additionalInfo, scopeNames);
        }

        private JsglrCodeCompletionProposal createCompletionInsertAtEnd(String name, String text, String additionalInfo,
                                                        StrategoAppl change, boolean blankLineCompletion) {

            final IStrategoTerm oldNode = change.getSubterm(0);
            final IStrategoTerm newNode = change.getSubterm(1);

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
                tokenPosition = includeLayoutByIndex(tokenizer, oldListIA.getLeftToken(), -1);
                insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
            } else {
                // if list is not empty
                // insert after at end offset of the rightmost token of the element before the
                // completion
                IStrategoTerm elementBefore = oldNode.getSubterm(oldNode.getAllSubterms().length - 1);
                IToken leftToken = elementBefore.getAttachment(ImploderAttachment.TYPE).getLeftToken();
                IToken rightToken = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken();
                tokenPosition = excludeLayoutByIndex(tokenizer, rightToken, leftToken, -1);
                insertionPoint = tokenizer.getTokenAt(tokenPosition).getEndOffset();
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

            return createProposal(text, sort, name, insertionPoint, suffixPoint, additionalInfo,
                    new ScopeNames(SpoofaxScopeNames.EXPANSION));
        }

        private JsglrCodeCompletionProposal createCompletionInsertBefore(String name, String text, String additionalInfo,
                                                         StrategoAppl change) {

            final IStrategoTerm oldNode = change.getSubterm(0);
            final IStrategoTerm newNode = change.getSubterm(1);

            if(change.getSubtermCount() != 2
                    || !(oldNode instanceof IStrategoAppl)
                    || !(newNode instanceof IStrategoList)) {
                return null;
            }

            // expect two terms and 1st should be an element of a list
            final IStrategoTerm oldList = ParentAttachment.getParent(oldNode);

            if (!(oldList instanceof IStrategoList)) {
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
            IToken leftToken = oldNode.getAttachment(ImploderAttachment.TYPE).getLeftToken();
            if(indexOfElement == 0) {
                // insert after the first non-layout token before the leftmost token of the
                // completion node
                insertionPoint = includeLayout(tokenizer, leftToken, -1).getEndOffset();
            } else {
                // if inserted element is not first
                // insert after at end offset of the rightmost token of the element before the
                // completion
                IStrategoTerm elementBefore = oldList.getSubterm(indexOfElement - 1);
                IToken rightToken = elementBefore.getAttachment(ImploderAttachment.TYPE).getRightToken();
                insertionPoint = rightToken.getEndOffset();
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

            IToken checkToken = leftToken;
            int checkTokenIdx = leftToken.getIndex();
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

            return createProposal(text, sort, name, insertionPoint, suffixPoint, additionalInfo,
                    new ScopeNames(SpoofaxScopeNames.EXPANSION));
        }

        private JsglrCodeCompletionProposal createProposal(
                String text,
                String sort,
                String name,
                int insertionPoint,
                int suffixPoint,
                String additionalInfo,
                ScopeNames scopeNames) {
            // TODO: Start and end offset.
            // TODO: Use additionalInfo
            int startOffset = insertionPoint + 1;
            int endOffset = suffixPoint;
            return new JsglrCodeCompletionProposal(text, sort, name, null, null, scopeNames, null);
        }

        /**
         * Gets the parent of the placeholder, or the placeholder itself when it has no parent.
         *
         * @param placeholder The placeholder term.
         * @return The placeholder's parent, or the placeholder itself.
         */
        private IStrategoTerm getPlaceholderOrParent(IStrategoAppl placeholder) {
            IStrategoTerm placeholderParent = ParentAttachment.getParent(placeholder);
            if (placeholderParent != null) {
                return placeholderParent;
            } else {
                return placeholder;
            }
        }

        /**
         * Gets the first index of the placeholder in the placeholder's parent, or -1 when not found.
         *
         * @param placeholderParent The placeholder parent.
         * @param placeholder The placeholder to look for.
         * @return The zero-based index of the placeholder subterm; or -1 when not found.
         */
        private int getPlaceholderIndex(IStrategoTerm placeholderParent, IStrategoAppl placeholder) {
            for (int i = 0; i < placeholderParent.getSubtermCount(); i++) {
                if (placeholderParent.getSubterm(i) == placeholder) {
                    return i;
                }
            }
            return -1;
        }

    }
}

