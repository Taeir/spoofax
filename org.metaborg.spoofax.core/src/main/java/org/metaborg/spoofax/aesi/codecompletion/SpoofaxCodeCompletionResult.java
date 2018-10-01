package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.SourceSpan;
import org.metaborg.aesi.SourceToken;
import org.metaborg.aesi.codecompletion.ICodeCompletionResult;

import java.util.List;

/**
 * A code completion result.
 */
public class SpoofaxCodeCompletionResult implements ISpoofaxCodeCompletionResult {

    /**
     * Initializes a new instance of the {@link SpoofaxCodeCompletionResult} class.
     *
     * @param prefix The span of the prefix.
     * @param proposals The list of proposals, in the order in which they are displayed.
     */
    public SpoofaxCodeCompletionResult(
            SourceToken prefix,
            List<SpoofaxCodeCompletionProposal> proposals
    ) {
        if (prefix == null) throw new IllegalArgumentException("prefix must not be null.");
        if (proposals == null) throw new IllegalArgumentException("proposals must not be null.");

        this.prefix = prefix;
        this.proposals = proposals;
    }

    private final SourceToken prefix;
    @Override
    public SourceSpan getPrefix() { return this.prefix.getSpan(); }

    private final List<SpoofaxCodeCompletionProposal> proposals;
    @Override
    public List<SpoofaxCodeCompletionProposal> getProposals() { return this.proposals; }

}
