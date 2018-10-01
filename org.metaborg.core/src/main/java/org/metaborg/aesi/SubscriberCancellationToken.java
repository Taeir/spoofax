package org.metaborg.aesi;

import rx.Subscriber;

/**
 * A cancellation token tied to an observable's subscription.
 */
public final class SubscriberCancellationToken implements ICancellationToken {

    private final Subscriber<? super Void> subscriber;

    /**
     * Initializes a new instance of the {@link SubscriberCancellationToken} class.
     *
     * @param subscriber The subscriber.
     */
    public SubscriberCancellationToken(Subscriber<? super Void> subscriber) {
        if (subscriber == null) throw new IllegalArgumentException("subscriber should not be null.");

        this.subscriber = subscriber;
    }

    @Override
    public boolean isCancelled() {
        return this.subscriber.isUnsubscribed();
    }

}
