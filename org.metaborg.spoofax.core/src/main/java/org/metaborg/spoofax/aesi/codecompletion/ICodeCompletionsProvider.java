package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.SourceToken;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;

import javax.annotation.Nullable;
import java.util.List;

/**
 * Provides syntactic code completion proposals.
 */
public interface ICodeCompletionsProvider {

    /**
     * Gets the completion proposals that apply to the specified offset
     * in the specified source unit.
     *
     * @param parseResult The parse result; or null when not available.
     * @param analysisResult The analysis result; or null when not available.
     * @param caretOffset The zero-based character offset in the source unit.
     * @param prefix The prefix, or an empty string when there is no prefix.
     * @return A list of code completion proposals in the relevant order.
     */
    List<SpoofaxCodeCompletionProposal> getCompletions(
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            SourceToken prefix);

}
