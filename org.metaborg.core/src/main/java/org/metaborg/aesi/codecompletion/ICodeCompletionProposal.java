package org.metaborg.aesi.codecompletion;

import org.metaborg.aesi.ScopeNames;

import javax.annotation.Nullable;
import java.io.Serializable;
import java.util.Set;

/**
 * A code completion proposal.
 */
public interface ICodeCompletionProposal {

    /**
     * Gets the content that is inserted for this completion proposal.
     *
     * @return The proposal's content.
     */
    String getContent();

    /**
     * Gets the label.
     *
     * @return The label; or null.
     */
    @Nullable
    String getLabel();

    /**
     * Gets the description.
     *
     * @return The description; or null.
     */
    @Nullable
    String getDescription();

    /**
     * Gets the right label.
     *
     * @return The right label; or null.
     */
    @Nullable
    String getRightLabel();

    /**
     * Gets the set of scope names.
     *
     * @return The set of scope names.
     */
    ScopeNames getScopeNames();

    /**
     * Gets the language and context-specific set of characters
     * that can be typed to commit to a proposal.
     *
     * @return A set of characters.
     */
    Set<Character> getCommitCharacters();

}
