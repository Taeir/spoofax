package org.metaborg.core.completions2;

/**
 * Configuration of the code completion service.
 * 
 * This configuration is sent by the editor to the service to
 * configure it correctly.
 */
public interface ICodeCompletionConfiguration {
	
	/**
	 * Gets whether the code completion service should return code snippets.
	 * 
	 * @return True when the service should include code snippets in the result; otherwise, False.
	 */
	boolean getIncludeSnippets();
}
