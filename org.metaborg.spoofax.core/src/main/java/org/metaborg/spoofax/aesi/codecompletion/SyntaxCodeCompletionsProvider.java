package org.metaborg.spoofax.aesi.codecompletion;

import com.google.inject.Inject;
import org.metaborg.aesi.ScopeNames;
import org.metaborg.aesi.SourceToken;
import org.metaborg.core.resource.IResourceService;
import org.metaborg.spoofax.core.stratego.IStrategoCommon;
import org.metaborg.spoofax.core.stratego.IStrategoRuntimeService;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.tracing.ISpoofaxTracingService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.spoofax.interpreter.terms.ITermFactory;
import org.strategoxt.HybridInterpreter;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Provides syntactic code completion.
 */
public class SyntaxCodeCompletionsProvider extends SpoofaxCodeCompletionsProvider {

    /**
     * Initializes a new instance of the {@link SpoofaxCodeCompletionsProvider} class.
     *
     * @param strategoCommon
     * @param termFactoryService
     * @param strategoRuntimeService
     * @param resourceService
     * @param tracingService
     */
    @Inject
    protected SyntaxCodeCompletionsProvider(IStrategoCommon strategoCommon, ITermFactoryService termFactoryService, IStrategoRuntimeService strategoRuntimeService, IResourceService resourceService, ISpoofaxTracingService tracingService) {
        super(strategoCommon, termFactoryService, strategoRuntimeService, resourceService, tracingService);
    }

    @Override
    protected Collection<SpoofaxCodeCompletionProposal> gatherCompletionsForComponent(@Nullable ISpoofaxParseUnit parseResult, @Nullable ISpoofaxAnalyzeUnit analysisResult, int caretOffset, HybridInterpreter runtime, ITermFactory termFactory) {
        return null;
    }
}

