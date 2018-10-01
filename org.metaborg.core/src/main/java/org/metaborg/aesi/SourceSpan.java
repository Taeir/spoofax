package org.metaborg.aesi;

import java.util.Objects;

/**
 * A span of source text.
 */
public final class SourceSpan {

    /**
     * Initializes a new instance of the {@link SourceSpan} class.
     *
     * @param start The inclusive start of the span.
     * @param end The exclusive end of the span.
     */
    public SourceSpan(SourceOffset start, SourceOffset end) {
        if (start == null) throw new IllegalArgumentException("start must not be null.");
        if (end == null) throw new IllegalArgumentException("end must not be null.");
        if (start.compareTo(end) < 0) throw new IllegalArgumentException("start must be at or after end.");

        this.start = start;
        this.end = end;
    }

    /**
     * Initializes a new instance of the {@link SourceSpan} class.
     */
    public SourceSpan() { this(new SourceOffset(), new SourceOffset()); }

    private final SourceOffset start;
    /**
     * Gets the inclusive start of the span.
     *
     * @return The start of the span.
     */
    public SourceOffset getStart() { return this.start; }

    private final SourceOffset end;
    /**
     * Gets the exclusive end of the span.
     *
     * @return The end of the span.
     */
    public SourceOffset getEnd() { return this.end; }

    /**
     * Gets whether the span is empty.
     *
     * @return True when the span is empty;
     * otherwise, False.
     */
    public boolean isEmpty() { return getLength() == 0; }

    /**
     * Gets the number of characters in the span.
     *
     * @return The number of characters in the span.
     */
    public int getLength() { return this.end.minus(this.start); }

    /**
     * Returns whether the span contains the specified offset.
     *
     * Note that an empty span contains no offsets.
     *
     * @param offset The offset to check.
     * @return True when the specified offset is in this span;
     * otherwise, False.
     */
    public boolean contains(SourceOffset offset) {
        if (offset == null) throw new IllegalArgumentException("offset must not be null.");

        return this.start.getValue() <= offset.getValue()
            && offset.getValue() < this.end.getValue();
    }

    /**
     * Returns whether the specified span intersects this span.
     *
     * Note that if either span is empty, this method returns False.
     *
     * @param other The other span to check.
     * @return True when (part of) the this span and the specified span overlap;
     * otherwise, False.
     */
    public boolean intersects(SourceSpan other) {
        if (other == null) throw new IllegalArgumentException("other must not be null.");

        return !this.isEmpty() && !other.isEmpty()
            && this.start.getValue() < other.end.getValue()
            && other.start.getValue() < this.end.getValue();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SourceSpan
            && equals((SourceSpan)obj);
    }

    public boolean equals(SourceSpan other) {
        return other != null
            && this.start.equals(other.start)
            && this.end.equals(other.end);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                this.start,
                this.end
        );
    }

    @Override
    public String toString() {
        return String.format("@%d-%d", this.start.getValue(), this.end.getValue());
    }
}
