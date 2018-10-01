package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.codecompletion.ICodeCompletionProposal;
import org.metaborg.aesi.codecompletion.ICodeCompletionResult;

import java.util.List;

public interface ISpoofaxCodeCompletionResult extends ICodeCompletionResult {

    /**
     * Gets the code completion proposals.
     *
     * @return An ordered list of code completion proposals.
     */
    List<? extends ISpoofaxCodeCompletionProposal> getProposals();

}
