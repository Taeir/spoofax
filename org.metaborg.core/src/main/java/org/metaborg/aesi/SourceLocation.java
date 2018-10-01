package org.metaborg.aesi;

import javax.annotation.Nullable;
import java.util.Objects;

/**
 * A source text location, as a zero-based line number and character on that line.
 */
public final class SourceLocation {

    /**
     * Initializes a new instance of the {@link SourceLocation} class.
     *
     * @param line The one-based line number.
     * @param character The one-based character offset from the start of the line.
     */
    public SourceLocation(int line, int character) {
        if (line <= 0) throw new IllegalArgumentException("line must be positive non-zero.");
        if (character <= 0) throw new IllegalArgumentException("character must be positive non-zero.");

        this.line = line;
        this.character = character;
    }

    /**
     * Initializes a new instance of the {@link SourceLocation} class.
     */
    public SourceLocation() { this(1, 1); }

    private final int line;
    /**
     * Gets the one-based line number.
     * @return The line number.
     */
    public int getLine() { return this.line; }

    private final int character;
    /**
     * Gets the one-based character offset on the line.
     * @return The character offset.
     */
    public int getCharacter() { return this.character; }

    @Override
    public boolean equals(@Nullable Object obj) {
        return obj instanceof SourceLocation
            && equals((SourceLocation)obj);
    }

    public boolean equals(@Nullable SourceLocation other) {
        return other != null
            && this.line == other.line
            && this.character == other.character;
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                this.line,
                this.character
        );
    }

    @Override
    public String toString() {
        return String.format("%d:%d", this.line, this.character);
    }

}
