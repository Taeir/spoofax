package org.metaborg.aesi.resources;

/**
 * Specifies the kind of resource.
 */
public enum ResourceKind {
    /**
     * The resource is a file.
     */
    File,
    /**
     * The resource is a folder.
     */
    Folder,
    /**
     * The resource is a project.
     */
    Project,
    /**
     * The resource is a workspace.
     */
    Workspace
}
