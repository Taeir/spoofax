package org.metaborg.spoofax.core.completion;

/**
 * Specifies what to include in the returned completions.
 */
public enum CompletionOptions {
    /**
     * Propose syntax.
     */
    Syntax,
    /**
     * Propose accessible types (e.g. classes, interfaces, traits).
     */
    Types,
    /**
     * Propose accessible members (e.g. fields, methods, variables).
     */
    Members,
    /**
     * Propose inaccessible types (e.g. types that need an import).
     */
    InaccessibleTypes,
    /**
     * Propose inaccessible members (e.g. members that need an import).
     */
    InaccessibleMembers,
    /**
     * Only include type-compatible proposals.
     */
    TypeSmart
}
