package org.metaborg.aesi.codecompletion;

import org.metaborg.aesi.ICancellationToken;
import org.metaborg.aesi.SourceOffset;

import javax.annotation.Nullable;
import java.net.URI;

/**
 * Code completion service.
 */
public interface ICodeCompletionService {

    /**
     * Gets the code completion proposals that are relevant
     * at the specified caret position in the specified document.
     *
     * @param resourceUri The resource URI.
     * @param caret The caret position.
     * @param cancellationToken The cancellation token; or null.
     * @return The code completion result; or null.
     */
    @Nullable
    ICodeCompletionResult getCompletions(
            URI resourceUri,
            SourceOffset caret,
            @Nullable ICancellationToken cancellationToken);

}
