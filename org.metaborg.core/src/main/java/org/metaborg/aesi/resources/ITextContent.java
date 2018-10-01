package org.metaborg.aesi.resources;

/**
 * Resource content.
 */
public interface ITextContent {

    /**
     * Gets the length of the content.
     *
     * @return The length of the content, in bytes.
     */
    long getLength();

    /**
     * Gets the version stamp.
     *
     * Two contents are different iff their version stamps are different.
     * Note that if two contents have different version stamps,
     * their content can still be the same.
     *
     * @return The version stamp.
     */
    long getVersionStamp();

    /**
     * Gets the number of lines in the content.
     *
     * @return The number of lines.
     */
    int getLineCount();

    /**
     * Gets the text content.
     *
     * @return The text.
     */
    String getText();

}
