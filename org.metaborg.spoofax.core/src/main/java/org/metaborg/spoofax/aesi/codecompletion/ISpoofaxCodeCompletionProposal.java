package org.metaborg.spoofax.aesi.codecompletion;

import org.metaborg.aesi.codecompletion.ICodeCompletionProposal;

import java.io.Serializable;

/**
 * A Spoofax code completion proposal.
 */
public interface ISpoofaxCodeCompletionProposal extends ICodeCompletionProposal, Serializable {
    // Serializable because it is necessary to pass an object as a String to Eclipse additional info menu.

}
