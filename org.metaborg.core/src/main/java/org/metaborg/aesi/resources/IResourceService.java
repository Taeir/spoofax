package org.metaborg.aesi.resources;

import javax.annotation.Nullable;
import java.net.URI;
import java.util.Collection;

/**
 * The AESI resource service.
 */
public interface IResourceService {

    /**
     * Gets the workspace root resource.
     *
     * @return The workspace resource.
     */
    IResource getWorkspace();

    /**
     * Resolves the specified URI to a resource, if possible.
     *
     * The returned resource may or may not exist.
     *
     * @param resourceUri The URI of the resource to resolve.
     * @return The corresponding resource; or null if not found.
     */
    @Nullable IResource resolve(URI resourceUri);

    /**
     * Determines whether the resource with the specified URI currently exists.
     *
     * @param resourceUri The URI of the resource to check.
     * @return True when the resource currently exists; otherwise, False.
     */
    boolean exists(URI resourceUri);

    /**
     * Gets the parent resource of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return The parent resource; or null when it has none or it could not be determined.
     */
    @Nullable IResource getParentOf(URI resourceUri);

    /**
     * Gets the child resources of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return A collection of child resources.
     */
    Collection<? extends IResource> getChildrenOf(URI resourceUri);

    /**
     * Gets the content of the specified resource.
     *
     * @param resourceUri The URI of the resource to check.
     * @return The content of the resource; or null.
     */
    @Nullable
    ITextContent getContentOf(URI resourceUri);

//    @Nullable SourceLocation getLocationAt(ITextContent content, SourceOffset offset);
//    @Nullable SourdeOffset getOffsetAt(ITextContent content, SourceLocation location);

}
