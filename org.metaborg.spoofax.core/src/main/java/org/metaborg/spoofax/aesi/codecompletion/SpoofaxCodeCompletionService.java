package org.metaborg.spoofax.aesi.codecompletion;

import com.google.inject.Inject;
import org.metaborg.aesi.ICancellationToken;
import org.metaborg.aesi.SourceOffset;
import org.metaborg.aesi.SourceToken;
import org.metaborg.aesi.codecompletion.ICodeCompletionService;
import org.metaborg.core.context.ContextException;
import org.metaborg.core.context.IContext;
import org.metaborg.core.context.IContextService;
import org.metaborg.core.processing.analyze.IAnalysisResultRequester;
import org.metaborg.core.processing.parse.IParseResultRequester;
import org.metaborg.core.project.IProject;
import org.metaborg.core.project.IProjectService;
import org.metaborg.core.unit.IInputUnitService;
import org.metaborg.spoofax.aesi.resources.ISpoofaxResource;
import org.metaborg.spoofax.aesi.resources.ISpoofaxResourceService;
import org.metaborg.spoofax.aesi.resources.ISpoofaxTextContent;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxInputUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;

import javax.annotation.Nullable;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * The Spoofax code completion service.
 */
public class SpoofaxCodeCompletionService implements ISpoofaxCodeCompletionService {

    private final Set<ICodeCompletionsProvider> providers;
    private final ISpoofaxResourceService resourceService;
    private final IInputUnitService<ISpoofaxInputUnit> unitService;
    private final IParseResultRequester<ISpoofaxInputUnit, ISpoofaxParseUnit> parseResultRequester;
    private final IAnalysisResultRequester<ISpoofaxInputUnit, ISpoofaxAnalyzeUnit> analysisResultRequester;
    private final IContextService contextService;
    private final IProjectService projectService;

    /**
     * Initializes a new instance of the {@link SpoofaxCodeCompletionService} class.
     *
     * @param providers The code completions providers.
     */
    @Inject
    public SpoofaxCodeCompletionService(
            Set<ICodeCompletionsProvider> providers,
            ISpoofaxResourceService resourceService,
            IInputUnitService<ISpoofaxInputUnit> unitService,
            IParseResultRequester<ISpoofaxInputUnit, ISpoofaxParseUnit> parseResultRequester,
            IAnalysisResultRequester<ISpoofaxInputUnit, ISpoofaxAnalyzeUnit> analysisResultRequester,
            IContextService contextService,
            IProjectService projectService
    ) {
        assert providers != null;
        assert resourceService != null;
        assert unitService != null;
        assert parseResultRequester != null;
        assert analysisResultRequester != null;
        assert contextService != null;
        assert projectService != null;

        this.providers = providers;
        this.resourceService = resourceService;
        this.unitService = unitService;
        this.parseResultRequester = parseResultRequester;
        this.analysisResultRequester = analysisResultRequester;
        this.contextService = contextService;
        this.projectService = projectService;
    }

    @Override
    public SpoofaxCodeCompletionResult getCompletions(
            URI resourceUri,
            SourceOffset caret,
            @Nullable ICancellationToken cancellationToken
    ) {
        if (resourceUri == null) throw new IllegalArgumentException("document must not be null.");
        if (caret == null) throw new IllegalArgumentException("caret must not be null.");

        int offset = caret.getValue();

        ISpoofaxParseUnit parseResult = getParseResult(resourceUri, true);
        ISpoofaxAnalyzeUnit analysisResult = getAnalysisResult(resourceUri, false);
        SourceToken prefix = determinePrefix(parseResult, offset);
        ArrayList<SpoofaxCodeCompletionProposal> proposals = gatherProposals(parseResult, analysisResult, offset, prefix, cancellationToken);
        orderProposalsByRelevance(proposals);

        return new SpoofaxCodeCompletionResult(prefix, proposals);
    }

    /**
     * Gets the parse result for the specified URI.
     *
     * @param resourceUri The resource URI.
     * @param force Force the parsing if it is invalidated or not available.
     * @return The parse result; or null when it could not be determined.
     */
    @Nullable
    private ISpoofaxParseUnit getParseResult(URI resourceUri, boolean force) {
        assert resourceUri != null;

        @Nullable ISpoofaxResource resource = this.resourceService.resolve(resourceUri);
        if (resource == null) return null;

        if (force) {
            @Nullable ISpoofaxTextContent content = this.resourceService.getContentOf(resourceUri);
            if (content == null) return null;

            final ISpoofaxInputUnit input = this.unitService.inputUnit(resource.getFile(), content.getText(),
                    resource.getLanguage(), null);
            return this.parseResultRequester.request(input).toBlocking().first();
        } else {
            return this.parseResultRequester.get(resource.getFile());
        }
    }

    /**
     * Gets the analysis result for the specified URI.
     *
     * @param resourceUri The resource URI.
     * @param force Force the analysis if it is invalidated or not available.
     * @return The analysis result; or null when it could not be determined.
     */
    @Nullable
    private ISpoofaxAnalyzeUnit getAnalysisResult(URI resourceUri, boolean force) {
        assert resourceUri != null;

        @Nullable ISpoofaxResource resource = this.resourceService.resolve(resourceUri);
        if (resource == null) return null;

        if (force) {
            @Nullable ISpoofaxTextContent content = this.resourceService.getContentOf(resourceUri);
            if (content == null) return null;
            @Nullable final IProject project = this.projectService.get(resource.getFile());
            if (project == null) return null;

            final ISpoofaxInputUnit input = this.unitService.inputUnit(resource.getFile(), content.getText(),
                    resource.getLanguage(), null);
            final IContext context;
            try {
                context = this.contextService.get(resource.getFile(), project, resource.getLanguage());
            } catch (ContextException e) {
                return null;
            }
            return this.analysisResultRequester.request(input, context).toBlocking().first();
        } else {
            return this.analysisResultRequester.get(resource.getFile());
        }
    }

    /**
     * Determines the prefix at the caret offset.
     *
     * @param unit The source unit.
     * @param offset The zero-based character offset in the source unit.
     * @return The prefix token, or an empty string.
     */
    private SourceToken determinePrefix(ISpoofaxParseUnit unit, int offset) {
        assert unit != null;

        // TODO
        return new SourceToken();
    }

    /**
     * Gathers the code completion proposals.
     *
     * @param parseResult The parse result.
     * @param analysisResult The analysis result; or null.
     * @param offset The zero-based character offset in the source unit.
     * @param prefix The prefix; or an empty string.
     * @param cancellationToken The cancellation token; or null.
     * @return A list of code completion proposals.
     */
    private ArrayList<SpoofaxCodeCompletionProposal> gatherProposals(
            ISpoofaxParseUnit parseResult,
            @Nullable ISpoofaxAnalyzeUnit analysisResult,
            int offset,
            SourceToken prefix,
            @Nullable ICancellationToken cancellationToken
    ) {
        assert prefix != null;

        ArrayList<SpoofaxCodeCompletionProposal> proposals = new ArrayList<>();
        for (ICodeCompletionsProvider provider : this.providers) {
            if (cancellationToken != null && cancellationToken.isCancelled()) break;

            List<SpoofaxCodeCompletionProposal> completions = provider.getCompletions(parseResult, analysisResult, offset, prefix);
            proposals.addAll(completions);
        }
        return proposals;
    }

    /**
     * Orders the proposals by relevance.
     *
     * @param proposals The proposals to order in-place.
     */
    private void orderProposalsByRelevance(ArrayList<SpoofaxCodeCompletionProposal> proposals) {
        assert proposals != null;

        // Nothing to do.
    }

}
