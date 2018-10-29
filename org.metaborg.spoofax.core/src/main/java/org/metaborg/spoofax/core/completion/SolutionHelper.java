package org.metaborg.spoofax.core.completion;

import mb.nabl2.constraints.ast.AstProperties;
import mb.nabl2.scopegraph.terms.Occurrence;
import mb.nabl2.scopegraph.terms.Scope;
import mb.nabl2.solver.ISolution;
import mb.nabl2.stratego.ConstraintTerms;
import mb.nabl2.stratego.StrategoTerms;
import mb.nabl2.stratego.TermIndex;
import mb.nabl2.terms.ITerm;
import mb.nabl2.terms.matching.Transform;
import mb.nabl2.terms.unification.IUnifier;
import org.metaborg.core.language.ILanguageComponent;
import org.metaborg.core.language.ILanguageImpl;
import org.spoofax.interpreter.terms.ITermFactory;
import org.strategoxt.HybridInterpreter;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

/**
 * Helper functions for querying the {@link ISolution}.
 */
public final class SolutionHelper {

//    private final ILanguageImpl language;
//    private final ILanguageComponent languageComponent;
//    private final ITermFactory termFactory;
//    private final HybridInterpreter runtime;
//    private final StrategoTerms strategoTerms;
    private final ISolution solution;

    /**
     * Initializes a new instance of the SolutionHelper class.
     *
     * @param solution The solution.
//     * @param language The language.
//     * @param termFactory The term factory.
//     * @param runtime The Stratego runtime.
//     * @param strategoTerms The Stratego/Nabl2 terms helper.
     */
    public SolutionHelper(
            ISolution solution/*,
            ILanguageImpl language,
            ILanguageComponent languageComponent,
            ITermFactory termFactory,
            HybridInterpreter runtime,
            StrategoTerms strategoTerms*/) {
        if (solution == null) throw new IllegalArgumentException("solution must not be null.");
//        if (language == null) throw new IllegalArgumentException("language must not be null.");
//        if (languageComponent == null) throw new IllegalArgumentException("languageComponent must not be null.");
//        if (termFactory == null) throw new IllegalArgumentException("termFactory must not be null.");
//        if (runtime == null) throw new IllegalArgumentException("runtime must not be null.");
//        if (strategoTerms == null) throw new IllegalArgumentException("strategoTerms must not be null.");

        this.solution = solution;
//        this.language = language;
//        this.languageComponent = languageComponent;
//        this.termFactory = termFactory;
//        this.runtime = runtime;
//        this.strategoTerms = strategoTerms;
    }

    /**
     * Gets the name occurrences visible from the specified scope.
     *
     * @param scope The scope.
     * @return The set of occurrences, which may be empty.
     */
    public Set<Occurrence> getOccurrencesInScope(Scope scope) {
        if (scope == null) throw new IllegalArgumentException("scope must not be null.");

        return this.solution.nameResolution()
                .visible(scope)
                .orElse(Collections.emptySet());
    }

    /**
     * Gets the scopes that are parameters to the rule that applies to the specified term.
     *
     * @param termIndex The term index of a term.
     * @return The collection of scopes, which may be empty.
     */
    public Collection<Scope> getScopesOfTerm(TermIndex termIndex) {
        if (termIndex == null) throw new IllegalArgumentException("termIndex must not be null.");

        @Nullable final ITerm params = getParamsOfTerm(termIndex);
        if (params == null) return Collections.emptyList();

        // Get the scopes from the params.
        final IUnifier unifier = this.solution.unifier();
        return Transform.T.collecttd(t -> Scope.matcher().match(t, unifier)).apply(params);
    }

    /**
     * Gets the type of the specified term.
     *
     * @param termIndex The term index of a term.
     * @return The type; or null.
     */
    @Nullable public ITerm getParamsOfTerm(TermIndex termIndex) {
        if (termIndex == null) throw new IllegalArgumentException("termIndex must not be null.");

        // Get the term's [[ _ ^ params : _ ]].
        @Nullable final ITerm params = this.solution.astProperties()
                .getValue(termIndex, AstProperties.PARAMS_KEY)
                .orElse(null);
        return params;
    }

    /**
     * Gets the type of the specified term.
     *
     * @param termIndex The term index of a term.
     * @return The type; or null.
     */
    @Nullable public ITerm getTypeOfTerm(TermIndex termIndex) {
        if (termIndex == null) throw new IllegalArgumentException("termIndex must not be null.");

        // Get the term's [[ _ ^ _ : type ]].
        @Nullable final ITerm type = this.solution.astProperties()
                .getValue(termIndex, AstProperties.TYPE_KEY)
                .orElse(null);
        return type;
    }

}
