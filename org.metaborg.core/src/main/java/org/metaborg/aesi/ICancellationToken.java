package org.metaborg.aesi;

/**
 * A cancellation token.
 */
public interface ICancellationToken {

    /**
     * Gets whether the operation has been cancelled.
     *
     * @return True when the operation has been cancelled;
     * otherwise, False.
     */
    boolean isCancelled();

}
