package org.metaborg.aesi.codecompletion;

import org.metaborg.aesi.SourceSpan;

import java.util.List;

/**
 * The result returned by the code completion service.
 */
public interface ICodeCompletionResult {

    /**
     * Gets the span of the common prefix for all code completion proposals.
     *
     * @return The span of the common prefix; or an empty span when there is none.
     */
    SourceSpan getPrefix();

    /**
     * Gets the code completion proposals.
     *
     * @return An ordered list of code completion proposals.
     */
    List<? extends ICodeCompletionProposal> getProposals();

}
