package org.metaborg.core.completions2;

import java.util.List;

/**
 * The result of invoking the code completion service.
 */
public interface ICodeCompletionResult {
	
	/**
	 * Gets the prefix up to the cursor that is being completed.
	 * 
	 * Some editors use this prefix to highlight the word being completed.
     * Should be an empty string when there is no prefix.
     * 
	 * @return The prefix of the word being completed; or an empty string when there is no prefix.
	 */
	String getPrefix();
	
	/**
     * Gets a list of completion proposals to show to the user.
     *
     * The list MUST be in the order that the proposals should be shown to the user.
     * The list SHOULD only contain applicable entries (e.g. filtered by prefix).
     * When there is only one proposal, the editor MAY insert
     * that proposal without displaying the list of choices to the user.
     * 
     * @return A list of proposals.
     */
	List<ICodeCompletionProposal> getProposals();

}
