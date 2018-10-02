package org.metaborg.spoofax.aesi.codecompletion;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
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
import org.metaborg.core.context.IContext;
import org.metaborg.core.resource.IResourceService;
import org.metaborg.spoofax.core.context.constraint.IConstraintContext;
import org.metaborg.spoofax.core.stratego.IStrategoCommon;
import org.metaborg.spoofax.core.stratego.IStrategoRuntimeService;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.tracing.ISpoofaxTracingService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.metaborg.util.functions.Function1;
import org.metaborg.util.task.NullCancel;
import org.metaborg.util.task.NullProgress;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.spoofax.interpreter.terms.IStrategoAppl;
import org.spoofax.interpreter.terms.IStrategoTerm;
import org.spoofax.interpreter.terms.ITermFactory;
import org.strategoxt.HybridInterpreter;

import javax.annotation.Nullable;
import java.util.*;

public final class NameCodeCompletionsProvider extends SpoofaxCodeCompletionsProvider {

    private static final Logger logger = LoggerFactory.getLogger(NameCodeCompletionsProvider.class);

    /**
     * Initializes a new instance of the {@link NameCodeCompletionsProvider} class.
     */
    @Inject
    public NameCodeCompletionsProvider(
            IStrategoCommon strategoCommon,
            ITermFactoryService termFactoryService,
            IStrategoRuntimeService strategoRuntimeService,
            IResourceService resourceService,
            ISpoofaxTracingService tracingService
    ) {
        super(
                strategoCommon,
                termFactoryService,
                strategoRuntimeService,
                resourceService,
                tracingService
        );
    }

//    private static class Completer {
//        HybridInterpreter runtime;
//        ITermFactory termFactory;
//        IStrategoCommon strategoCommon;
//        StrategoTerms strategoTerms;
//        CompletionSolver solver;
//        IConstraintContext context;
//        Function1<String, String> fresh;
//        IResult result;
//        ISolution solution;
//        IUnifier unifier;
//    }

    @Override
    protected Collection<SpoofaxCodeCompletionProposal> gatherCompletionsForComponent(
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            HybridInterpreter runtime,
            ITermFactory termFactory
    ) {
        assert analysisResult != null;

        StrategoTerms strategoTerms = new StrategoTerms(termFactory);
        CompletionSolver solver = new CompletionSolver(CallExternal.never());

        List<SpoofaxCodeCompletionProposal> proposals = new LinkedList<>();
        Map<IStrategoTerm, Boolean> leftRecursiveTerms = new HashMap<>();
        Map<IStrategoTerm, Boolean> rightRecursiveTerms = new HashMap<>();
        IStrategoTerm ast = analysisResult.ast();
        List<IStrategoTerm> terms = findAstTermsAtOffset(ast, caretOffset, runtime, termFactory, leftRecursiveTerms, rightRecursiveTerms);

        @Nullable IStrategoAppl placeholder = getPlaceholderFromTerms(terms, caretOffset);

        final IContext context = analysisResult.context();
        if (placeholder != null && context instanceof IConstraintContext) {
            // From the placeholder, the most specific element:
            logger.debug("Completing on placeholder: " + placeholder.toString());
            IConstraintContext ccontext = (IConstraintContext)context;

            @Nullable TermIndex termIndex = getTermIndex(placeholder);
            if (termIndex == null) return Collections.emptyList();
            @Nullable IResult result = getAnalysisResult(termIndex, ccontext);
            if (result == null) return Collections.emptyList();

            Function1<String, String> fresh = result.fresh().melt()::fresh;

            // Get the term's [[ _ ^ args : type ]].
            @Nullable ITerm args = result.solution().astProperties().getValue(termIndex, AstProperties.PARAMS_KEY).orElse(null);
            @Nullable ITerm type = result.solution().astProperties().getValue(termIndex, AstProperties.TYPE_KEY).orElse(null);
            if (args == null || type == null) return Collections.emptyList();

            // Get the scopes from the args.
            IUnifier unifier = result.solution().unifier();
            Collection<Scope> scopes = Transform.T.collecttd(t -> Scope.matcher().match(t, unifier)).apply(args);

            Set<Occurrence> occurrences = getOccurrencesInScopes(scopes, result.solution());
            for (IStrategoTerm syntaxFragment : terms) {
                List<SpoofaxCodeCompletionProposal> fragmentProposals = findSemanticCompletionsForFragment(syntaxFragment, args, type, fresh, result, occurrences, strategoTerms, solver);
                proposals.addAll(fragmentProposals);
            }
        } else {
            logger.debug("Completion not in placeholder.");
            final Iterable<IStrategoTerm> optionals = getOptionalsFromTerms(terms, leftRecursiveTerms, rightRecursiveTerms);
            final Iterable<IStrategoTerm> leftRecursive = getLeftRecursiveTermsFromTerms(terms, caretOffset, leftRecursiveTerms);
            final Iterable<IStrategoTerm> rightRecursive = getRightRecursiveTermsFromTerms(terms, caretOffset, rightRecursiveTerms);
            // TODO
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
     * @return The completion proposals.
     */
    private List<SpoofaxCodeCompletionProposal> findSemanticCompletionsForFragment(IStrategoTerm syntaxFragment, ITerm params, ITerm type, Function1<String, String> fresh, IResult result, Set<Occurrence> occurrences, StrategoTerms strategoTerms, CompletionSolver solver) {
        ArrayList<SpoofaxCodeCompletionProposal> proposals = Lists.newArrayList();
        for (Occurrence occurrence : occurrences) {
            // Build the fragment.
            IStrategoTerm fragment = buildFragment(syntaxFragment, occurrence, strategoTerms);

            boolean isValid = isSemanticallyValidFragment(fragment, params, type, result, fresh, strategoTerms, solver);

            if (isValid) {
                // Build a completion from it.
                SpoofaxCodeCompletionProposal proposal = buildCompletionProposal(fragment);
                proposals.add(proposal);
            }
        }

        return proposals;
    }

    /**
     * Builds a fragment from the specified syntax fragment and occurrence.
     *
     * @param syntaxFragment The syntax fragment.
     * @param occurrence The occurrence.
     * @return The built fragment with the name of the occurrence integrated into it; or null.
     */
    @Nullable private IStrategoTerm buildFragment(IStrategoTerm syntaxFragment, Occurrence occurrence, StrategoTerms strategoTerms) {
        // Get the name of the occurrence.
        ITerm nablName = occurrence.getName();
        IStrategoTerm strategoName = strategoTerms.toStratego(nablName);

//        // We can't do anything with placeholders.
//        if (isPlaceholderTerm(syntaxFragment)) return null;
//
//        if(syntaxFragment instanceof IStrategoAppl && appl.getConstructor().getName().endsWith("-Plhdr")) return null;

        // TODO
        throw new IllegalStateException();
    }

    /**
     * Determines whether the specified fragment is semantivally valid.
     *
     * @param fragment The fragment to test.
     * @param result The original analysis result.
     * @param fresh Function for fresh names.
     * @return True when the fragment is semantically valid;
     * otherwise, false.
     */
    private boolean isSemanticallyValidFragment(IStrategoTerm fragment, ITerm args, ITerm type, IResult result, Function1<String, String> fresh, StrategoTerms strategoTerms, CompletionSolver solver) {
        // Create the constraint [[ fragment ^ args : type ]] or [[ fragment ^ args ]] (when there is NoType).
        // args = Params(params) (in het geval van NoType()) of Params(params, type)
        // constraintTerm =  nabl2--generate-constraint-completion Tuple(strategoName, fragment, args)
        IStrategoTerm constraintTerm = null;
        ITerm constraintStrategoTerm = strategoTerms.fromStratego(constraintTerm);
        @Nullable IConstraint constraint = ConstraintTerms.specialize(Constraints.matcher()).match(constraintStrategoTerm).orElse(null);
        if (constraint == null) return false;
        List<IConstraint> completionConstraints = Collections.singletonList(constraint);

        // Create a new solution with the new constraints added to it.
        // We remove the messages from the solution so that we can see any errors resulting from adding the constraint.
        ISolution oldSolution = result.solution();
        oldSolution.withConstraints(completionConstraints).withMessages(Messages.Immutable.of());
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

    /**
     * Builds a completion proposal from the specified fragment.
     *
     * @param fragment The fragment.
     * @return The completion proposal.
     */
    private SpoofaxCodeCompletionProposal buildCompletionProposal(IStrategoTerm fragment) {
        return new SpoofaxCodeCompletionProposal(fragment.toString(), null, null, null, null, null);
    }

    /**
     * Gets the occurrences in the specified scopes.
     *
     * @param scopes The scopes to look into.
     * @param solution The solution to query.
     * @return A set of occurrences.
     */
    private Set<Occurrence> getOccurrencesInScopes(Iterable<Scope> scopes, ISolution solution) {
        HashSet<Occurrence> occurrences = Sets.newHashSet();
        for (Scope scope : scopes) {
            Set<Occurrence> scopeOccurrences = solution.nameResolution().visible(scope).orElse(Collections.emptySet());
            occurrences.addAll(scopeOccurrences);
        }
        return occurrences;
    }


}
