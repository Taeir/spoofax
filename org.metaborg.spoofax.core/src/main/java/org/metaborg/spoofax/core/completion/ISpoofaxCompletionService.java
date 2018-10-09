package org.metaborg.spoofax.core.completion;

import org.metaborg.core.MetaborgException;
import org.metaborg.core.completion.ICompletion;
import org.metaborg.spoofax.core.unit.ISpoofaxAnalyzeUnit;
import org.metaborg.spoofax.core.unit.ISpoofaxParseUnit;

/**
 * Typedef interface for a completion service with Spoofax interfaces.
 */
public interface ISpoofaxCompletionService {
    Iterable<ICompletion> get(int offset, ISpoofaxParseUnit parseUnit, ISpoofaxAnalyzeUnit analyzeUnit, boolean nested) throws MetaborgException;
}