package org.metaborg.spoofax.aesi.codecompletion;

import com.google.inject.Inject;
import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.SourceToken;
import org.metaborg.core.MetaborgException;
import org.metaborg.core.completion.CompletionKind;
import org.metaborg.core.completion.ICompletion;
import org.metaborg.spoofax.core.completion.JSGLRCompletionService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Adapter for the {@link JSGLRCompletionService} implementation.
 */
public class JSGLRCodeCompletionsProvider implements ICodeCompletionsProvider {

    private final JSGLRCompletionService completionService;

    /**
     * Initializes a new instance of the {@link JSGLRCodeCompletionsProvider} class.
     *
     * @param completionService The completions service.
     */
    @Inject
    public JSGLRCodeCompletionsProvider(
            JSGLRCompletionService completionService
    ) {
        assert completionService != null;

        this.completionService = completionService;
    }

    @Override
    public List<SpoofaxCodeCompletionProposal> getCompletions(
            @Nullable ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int caretOffset,
            SourceToken prefix) {
        if (parseResult == null) return Collections.emptyList();

        try {
            // FIXME: What is this 'nested' thing?
            Iterable<ICompletion> completions = completionService.get(caretOffset, parseResult, false);
            return StreamSupport.stream(completions.spliterator(), false)
                    .map(this::convertToProposal)
                    .collect(Collectors.toList());
        } catch (MetaborgException e) {
            return Collections.emptyList();
        }
    }

    /**
     * Converts the completion to an AESI completion proposal.
     *
     * @param completion The completion.
     * @return The completion proposal.
     */
    private SpoofaxCodeCompletionProposal convertToProposal(ICompletion completion) {
        return new SpoofaxCodeCompletionProposal(
                completion.text(),
                completion.name(),
                null,
                completion.suffix(),
                getScopeNames(completion.kind()),
                null
        );
    }

    /**
     * Gets the scope names associated with the specified completion kind.
     *
     * @param kind The kind of completion.
     * @return The scope names.
     */
    private ScopeNames getScopeNames(CompletionKind kind) {
        switch (kind) {
            case recovery: return new ScopeNames("recovery.spoofax");
            case expansion: return new ScopeNames("expansion.spoofax");
            case expansionEditing: return new ScopeNames("expansion.editing.spoofax");
            default: return new ScopeNames();
        }
    }
}
