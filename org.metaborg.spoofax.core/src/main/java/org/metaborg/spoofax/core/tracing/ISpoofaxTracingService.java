package org.metaborg.spoofax.core.tracing;

import org.metaborg.core.source.ISourceRegion;
import org.metaborg.core.tracing.ITracingService;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxTransformUnit;
import org.spoofax.interpreter.terms.IStrategoTerm;

/**
 * Typedef interface for {@link ITracingService} with Spoofax interfaces.
 */
public interface ISpoofaxTracingService
    extends ITracingService<ISpoofaxParseUnit, ISpoofaxAnalyzeUnit, ISpoofaxTransformUnit<?>, IStrategoTerm> {
    /**
     * @see #fragments
     */
    Iterable<IStrategoTerm> toTerms(IStrategoTerm ast, ISourceRegion region);

    /**
     * @see #fragmentsWithin
     */
    Iterable<IStrategoTerm> toTermsWithin(IStrategoTerm ast, ISourceRegion region);
}
