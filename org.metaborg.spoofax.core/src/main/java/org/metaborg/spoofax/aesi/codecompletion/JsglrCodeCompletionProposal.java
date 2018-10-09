package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.ScopeNames;

import javax.annotation.Nullable;
import java.util.Set;

/**
 * A JSGLR code completion proposal.
 */
public class JsglrCodeCompletionProposal extends SpoofaxCodeCompletionProposal {

    /**
     * Initializes a new instance of the {@link SpoofaxCodeCompletionProposal} class.
     *
     * @param content          The content of the proposal.
     * @param label            The label; or null.
     * @param description      The description; or null.
     * @param rightLabel       The right label; or null.
     * @param scopeNames       The scope names; or null.
     * @param commitCharacters The commit characters; or null.
     */
    public JsglrCodeCompletionProposal(
            String content,
            String sort,
            @Nullable String label,
            @Nullable String description,
            @Nullable String rightLabel,
            @Nullable ScopeNames scopeNames,
            @Nullable Set<Character> commitCharacters
    ) {
        super(content, label, description, rightLabel, scopeNames, commitCharacters);
        if (sort == null) throw new IllegalArgumentException("sort must not be null.");

        this.sort = sort;
    }

    private final String sort;

    /**
     * Gets the sort of this JSGLR code completion proposal.
     *
     * The sort is used as the start symbol when parsing the content of this proposal
     * for correct syntactic coloring in the preview window.
     *
     * @return The sort.
     */
    public String getSort() {
        return this.sort;
    }
}
