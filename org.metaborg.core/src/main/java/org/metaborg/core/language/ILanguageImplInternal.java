package org.metaborg.core.language;

public interface ILanguageImplInternal extends ILanguageImpl {
    /**
     * Adds a component to the implementation.
     * 
     * @param component
     *            Component to add.
     * @return True if the component was added, false if it already existed.
     */
    public abstract boolean addComponent(ILanguageComponent component);

    /**
     * Removes a component from the implementation.
     * 
     * @param component
     *            Component to remove.
     * @return True if the component was removed, false if it did not exist.
     */
    public abstract boolean removeComponent(ILanguageComponent component);

    /**
     * @return Language this implementation belongs to.
     */
    public abstract ILanguageInternal belongsToInternal();
}
