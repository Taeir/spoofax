package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.ICancellationToken;
import org.metaborg.aesi.SourceOffset;
import org.metaborg.aesi.codecompletion.ICodeCompletionResult;
import org.metaborg.aesi.codecompletion.ICodeCompletionService;

import javax.annotation.Nullable;
import java.net.URI;

public interface ISpoofaxCodeCompletionService extends ICodeCompletionService {

    /**
     * Gets the code completion proposals that are relevant at the specified caret position in the specified document.
     *
     * @param resourceUri The resource URI.
     * @param caret The caret position.
     * @param cancellationToken The cancellation token; or null.
     * @return The code completion result.
     */
    ISpoofaxCodeCompletionResult getCompletions(URI resourceUri, SourceOffset caret, @Nullable ICancellationToken cancellationToken);

}
