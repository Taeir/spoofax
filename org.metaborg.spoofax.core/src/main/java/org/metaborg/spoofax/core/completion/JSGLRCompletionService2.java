package org.metaborg.spoofax.core.completion;

import com.google.inject.Inject;
import org.metaborg.core.MetaborgException;
import org.metaborg.core.completion.ICompletion;
import org.metaborg.core.resource.IResourceService;
import org.metaborg.spoofax.core.stratego.IStrategoCommon;
import org.metaborg.spoofax.core.stratego.IStrategoRuntimeService;
import org.metaborg.spoofax.core.syntax.ISpoofaxSyntaxService;
import org.metaborg.spoofax.core.terms.ITermFactoryService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxUnitService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.EnumSet;
import java.util.List;

/**
 * Completion service.
 */
public final class JSGLRCompletionService2 implements ISpoofaxCompletionService {
    private static final Logger logger = LoggerFactory.getLogger(JSGLRCompletionService.class);

    private static final String PLACEHOLDER_SORT_SUFFIX = "-Plhdr";

    private final ITermFactoryService termFactoryService;
    private final IStrategoRuntimeService strategoRuntimeService;
    private final IStrategoCommon strategoCommon;
    private final IResourceService resourceService;
    private final ISpoofaxUnitService unitService;
    private final ISpoofaxSyntaxService syntaxService;


    @Inject
    public JSGLRCompletionService2(
            ITermFactoryService termFactoryService,
            IStrategoRuntimeService strategoRuntimeService,
            IStrategoCommon strategoCommon,
            IResourceService resourceService,
            ISpoofaxUnitService unitService,
            ISpoofaxSyntaxService syntaxService
    ) {
        this.termFactoryService = termFactoryService;
        this.strategoRuntimeService = strategoRuntimeService;
        this.strategoCommon = strategoCommon;
        this.resourceService = resourceService;
        this.unitService = unitService;
        this.syntaxService = syntaxService;
    }


    @Override
    public Iterable<ICompletion> get(int offset, ISpoofaxParseUnit parseUnit, ISpoofaxAnalyzeUnit analyzeUnit, boolean nested) throws MetaborgException {
        return null;
    }

    /**
     * Gets a list of completion proposals.
     *
     * @param completionOptions Specifies which proposals to include.
     * @return A list of completion proposals.
     */
    private List<ICompletion> getCompletions(EnumSet<CompletionOptions> completionOptions) {
        return null;
    }
}