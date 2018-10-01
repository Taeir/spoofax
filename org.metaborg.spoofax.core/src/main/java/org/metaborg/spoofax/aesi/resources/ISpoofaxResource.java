package org.metaborg.spoofax.aesi.resources;

import org.apache.commons.vfs2.FileObject;
import org.metaborg.aesi.resources.IResource;
import org.metaborg.core.language.ILanguageImpl;

import javax.annotation.Nullable;

/**
 * A Spoofax resource.
 */
public interface ISpoofaxResource extends IResource {

    /**
     * Gets the {@link FileObject} associated with the resource.
     *
     * @return The associated file object.
     */
    FileObject getFile();

    /**
     * Gets the language of the resource.
     *
     * @return The language; or null when it could not be determined.
     */
    @Nullable
    ILanguageImpl getLanguage();

}
