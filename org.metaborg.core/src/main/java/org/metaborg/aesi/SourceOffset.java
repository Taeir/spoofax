package org.metaborg.aesi;

import javax.annotation.Nullable;

/**
 * A source text offset, as a number of characters from the start of the source text.
 */
public final class SourceOffset implements Comparable<SourceOffset> {

    /**
     * Initializes a new instance of the {@link SourceOffset} class.
     *
     * @param value The offset value.
     */
    public SourceOffset(int value) {
        if (value < 0)
            throw new IllegalArgumentException("value must be positive or zero.");
        this.value = value;
    }

    /**
     * Initializes a new instance of the {@link SourceOffset} class.
     */
    public SourceOffset() { this(0); }

    private final int value;
    /**
     * Gets the value of the offset.
     * @return The value.
     */
    public int getValue() { return this.value; }

    /**
     * Returns a new offset incremented by 1.
     *
     * @return The new offset.
     */
    public SourceOffset inc() {
        return new SourceOffset(this.value + 1);
    }

    /**
     * Returns a new offset decremented by 1.
     *
     * @return The new offset.
     */
    public SourceOffset dec() {
        return new SourceOffset(this.value - 1);
    }

    /**
     * Adds the specified value to this offset.
     *
     * @param value The value to add.
     * @return The new offset.
     */
    public SourceOffset plus(int value) {
        int newValue = this.value + value;
        if (newValue < 0) throw new IllegalArgumentException("The resulting offset would be negative.");
        return new SourceOffset(newValue);
    }

    /**
     * Subtracts the specified value from this offset.
     *
     * @param value The value to subtract.
     * @return The new offset.
     */
    public SourceOffset minus(int value) {
        int newValue = this.value - value;
        if (newValue < 0) throw new IllegalArgumentException("The resulting offset would be negative.");
        return new SourceOffset(newValue);
    }

    /**
     * Subtracts the specified offset from this offset.
     *
     * @param offset The offset to subtract.
     * @return The difference.
     */
    public int minus(SourceOffset offset) {
        if (offset == null) throw new IllegalArgumentException("offset must not be null.");

        return this.value - offset.value;
    }

    @Override
    public int compareTo(@Nullable SourceOffset other) {
        if (other == null) return 1;
        return Integer.compare(this.value, other.value);
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        return obj instanceof SourceOffset
            && equals((SourceOffset)obj);
    }

    public boolean equals(@Nullable SourceOffset other) {
        return other != null
            && this.value == other.value;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(this.value);
    }

    @Override
    public String toString() {
        return String.format("@%d", this.value);
    }

}
