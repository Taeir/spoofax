package org.metaborg.core.project.config;

import java.io.IOException;

import javax.annotation.Nullable;

import org.metaborg.core.project.IProject;
import org.metaborg.util.file.FileAccess;

/**
 * Writes a configuration for an {@link IProject}.
 */
public interface IProjectConfigWriter {
    /**
     * Writes the specified configuration for the specified project.
     *
     * @param project
     *            The project.
     * @param config
     *            The configuration to write.
     * @param access
     */
    void write(IProject project, IProjectConfig config, @Nullable FileAccess access) throws IOException;
}
