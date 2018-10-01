package org.metaborg.aesi;

import com.google.common.collect.Sets;

import java.util.*;

/**
 * A set of scope names.
 */
public final class ScopeNames {

    /**
     * Initializes a new instance of the {@link ScopeNames} class.
     *
     * @param names The scope names.
     */
    public ScopeNames(Iterable<String> names) {
        if (names == null) throw new IllegalArgumentException("names must not be null.");

        this.names = Sets.newHashSet(names);
    }

    /**
     * Initializes a new instance of the {@link ScopeNames} class.
     *
     * @param names The scope names.
     */
    public ScopeNames(String... names) { this(Arrays.asList(names)); }

    private final Set<String> names;
    /**
     * Gets the names of the scopes.
     *
     * @return The set of names.
     */
    public Set<String> getNames() { return this.names; }

    /**
     * Determines whether there is any scope name in the set that has the specified prefix.
     *
     * @param prefix The prefix to look for.
     * @return True when such a prefix is found in the set of scope names;
     * otherwise, False.
     */
    public boolean contains(String prefix) {
        if (prefix == null) return false;

        for (String name : names) {
            if (name.equals(prefix) || name.startsWith(prefix + "."))
                return true;
        }
        return false;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ScopeNames
            && equals((ScopeNames)obj);
    }

    public boolean equals(ScopeNames other) {
        return other != null
            && this.names.equals(other.names);
    }

    @Override
    public int hashCode() {
        return this.names.hashCode();
    }

    @Override
    public String toString() {
        return String.join(" ", this.names);
    }
}
