package org.metaborg.aesi;

import java.util.Objects;

/**
 * A token from source text.
 */
public final class SourceToken {

    /**
     * Initializes a new instance of the {@link SourceToken} class.
     *
     * @param text The token text.
     * @param span The token span.
     */
    public SourceToken(String text, SourceSpan span) {
        if (text == null) throw new IllegalArgumentException("text must not be null");
        if (span == null) throw new IllegalArgumentException("span must not be null");

        this.text = text;
        this.span = span;
    }

    /**
     * Initializes a new instance of the {@link SourceToken} class.
     */
    public SourceToken() { this("", new SourceSpan()); }

    private final String text;
    /**
     * Gets the token text.
     *
     * @return The token text.
     */
    public String getText() { return this.text; }

    private final SourceSpan span;
    /**
     * Gets the token span.
     * @return The token span.
     */
    public SourceSpan getSpan() { return this.span; }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SourceToken
            && equals((SourceToken)obj);
    }

    public boolean equals(SourceToken other) {
        return other != null
            && this.text.equals(other.text)
            && this.span.equals(other.span);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                this.text,
                this.span
        );
    }

    @Override
    public String toString() {
        return this.text + this.span;
    }

}
