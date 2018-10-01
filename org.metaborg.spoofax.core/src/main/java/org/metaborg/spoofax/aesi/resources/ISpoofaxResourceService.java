package org.metaborg.spoofax.aesi.resources;

import org.metaborg.aesi.resources.IResource;
import org.metaborg.aesi.resources.IResourceService;
import org.metaborg.aesi.resources.ITextContent;
import org.metaborg.core.editor.IEditor;

import javax.annotation.Nullable;
import java.net.URI;
import java.util.Collection;

/**
 * The Spoofax resource service.
 */
public interface ISpoofaxResourceService extends IResourceService {

    /**
     * Gets the workspace root resource.
     *
     * @return The workspace resource.
     */
    ISpoofaxResource getWorkspace();

    /**
     * Resolves the specified URI to a resource, if possible.
     *
     * The returned resource may or may not exist.
     *
     * @param resourceUri The URI of the resource to resolve.
     * @return The corresponding resource; or null if not found.
     */
    @Nullable ISpoofaxResource resolve(URI resourceUri);

    /**
     * Gets the parent resource of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return The parent resource; or null when it has none or it could not be determined.
     */
    @Nullable ISpoofaxResource getParentOf(URI resourceUri);

    /**
     * Gets the child resources of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return A collection of child resources.
     */
    Collection<? extends ISpoofaxResource> getChildrenOf(URI resourceUri);

    /**
     * Gets the content of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return The content of the resource; or null.
     */
    @Nullable
    ISpoofaxTextContent getContentOf(URI resourceUri);

    /**
     * Gets the resource of the specified editor.
     *
     * @param editor The editor.
     * @return The resource.
     */
    ISpoofaxResource getResourceOf(IEditor editor);
}
