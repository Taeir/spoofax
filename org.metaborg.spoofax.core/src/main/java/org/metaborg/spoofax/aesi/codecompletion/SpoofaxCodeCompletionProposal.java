package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.codecompletion.ICodeCompletionProposal;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * A Spoofax code completion proposal.
 */
public class SpoofaxCodeCompletionProposal implements ISpoofaxCodeCompletionProposal {

    private static final long serialVersionUID = 602709401186409963L;

    /**
     * Initializes a new instance of the {@link SpoofaxCodeCompletionProposal} class.
     *
     * @param content The content of the proposal.
     * @param label The label; or null.
     * @param description The description; or null.
     * @param rightLabel The right label; or null.
     * @param scopeNames The scope names; or null.
     * @param commitCharacters The commit characters; or null.
     */
    public SpoofaxCodeCompletionProposal(
            String content,
            @Nullable String label,
            @Nullable String description,
            @Nullable String rightLabel,
            @Nullable ScopeNames scopeNames,
            @Nullable Set<Character> commitCharacters
    ) {
        if (content == null) throw new IllegalArgumentException("content must not be null.");
        if (scopeNames == null) scopeNames = new ScopeNames();
        if (commitCharacters == null) commitCharacters = Collections.emptySet();

        this.content = content;
        this.label = label;
        this.description = description;
        this.rightLabel = rightLabel;
        this.scopeNames = scopeNames;
        this.commitCharacters = commitCharacters;
    }

    private final String content;
    @Override
    public String getContent() { return this.content; }

    private final String label;
    @Nullable
    @Override
    public String getLabel() { return this.label; }

    private final String description;
    @Nullable
    @Override
    public String getDescription() { return this.description; }

    private final String rightLabel;
    @Nullable
    @Override
    public String getRightLabel() { return this.rightLabel; }

    private final ScopeNames scopeNames;
    @Override
    public ScopeNames getScopeNames() { return this.scopeNames; }

    private Set<Character> commitCharacters;
    @Override
    public Set<Character> getCommitCharacters() { return this.commitCharacters; }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SpoofaxCodeCompletionProposal
            && equals((SpoofaxCodeCompletionProposal)obj);
    }

    public boolean equals(SpoofaxCodeCompletionProposal other) {
        return other != null
            && this.content.equals(other.content);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                this.content
        );
    }

    @Override
    public String toString() {
        return this.content;
    }
}
