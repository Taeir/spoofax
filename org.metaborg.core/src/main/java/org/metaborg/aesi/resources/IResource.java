package org.metaborg.aesi.resources;

import javax.annotation.Nullable;
import java.net.URI;

/**
 * Represents a resource, such as a file, folder, project, or workspace.
 */
public interface IResource {

    /**
     * Gets the URI of the resource.
     *
     * @return The URI.
     */
    URI getUri();

    /**
     * Gets the name of the resource.
     *
     * The name includes the extension, if any.
     *
     * @return The name of the resource.
     */
    String getName();

    /**
     * Gets the kind of resource.
     *
     * @return A member of the {@link ResourceKind} enumeration.
     */
    ResourceKind getKind();

    /**
     * Gets whether the resource has any children.
     *
     * @return True when it has children; False when it has no children;
     * null when it is unknown whether it has children.
     */
    @Nullable
    Boolean hasChildren();

}
