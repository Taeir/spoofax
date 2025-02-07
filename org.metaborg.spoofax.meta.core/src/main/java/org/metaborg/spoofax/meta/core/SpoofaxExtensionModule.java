package org.metaborg.spoofax.meta.core;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.name.Names;
import org.metaborg.spoofax.core.Spoofax;
import org.metaborg.spoofax.core.SpoofaxModule;
import org.metaborg.spoofax.core.stratego.primitive.SpoofaxPrimitiveLibrary;
import org.metaborg.spoofax.core.stratego.primitive.legacy.LegacySpoofaxPrimitiveLibrary;
import org.metaborg.spoofax.meta.core.stratego.primitive.*;
import org.spoofax.interpreter.library.AbstractPrimitive;

/**
 * Module for extending {@link Spoofax}.
 * <p>
 * Note that this module is loaded together with a {@link SpoofaxModule}, so only those bindings are available. Bindings
 * from {@link SpoofaxMetaModule} are NOT available. To inject bindings from {@link SpoofaxMetaModule}, use static
 * injection and request injection in the {@link SpoofaxMetaModule}.
 * <p>
 * Static fields MUST BE SET TO NULL when the injector that loads the {@link SpoofaxModule} is closed, to avoid memory
 * leaks. This is done by implementing {@link AutoCloseable}, and adding classes that implement it to the auto closable
 * binder.
 */
public class SpoofaxExtensionModule extends AbstractModule {
    protected Multibinder<AutoCloseable> autoClosableBinder;
    protected Multibinder<AbstractPrimitive> spoofaxPrimitiveLibrary;
    protected Multibinder<AbstractPrimitive> legacySpoofaxPrimitiveLibrary;

    @Override protected void configure() {
        autoClosableBinder = Multibinder.newSetBinder(binder(), AutoCloseable.class);
        spoofaxPrimitiveLibrary = Multibinder.newSetBinder(binder(), AbstractPrimitive.class, Names.named(SpoofaxPrimitiveLibrary.name));
        legacySpoofaxPrimitiveLibrary = Multibinder.newSetBinder(binder(), AbstractPrimitive.class, Names.named(LegacySpoofaxPrimitiveLibrary.name));

        bindAutoClosableSpoofaxPrimitive(LanguageSpecificationPrimitive.class);
        bindAutoClosableSpoofaxPrimitive(LanguageSpecSrcGenDirectory.class);
        bindAutoClosableSpoofaxPrimitive(LanguageSpecPpNamePrimitive.class);
        bindAutoClosableSpoofaxPrimitive(CheckSdf2TablePrimitive.class);
        bindAutoClosableSpoofaxPrimitive(PlaceholderCharsPrimitive.class);
        bindSpoofaxPrimitive(LayoutSensitivePrettyPrinterPrimitive.class);
        bindSpoofaxPrimitive(GetContextualGrammarPrimitive.class);

        bindAutoClosableLegacySpoofaxPrimitive(LegacyLanguageSpecNamePrimitive.class);
    }


    protected <T extends AbstractPrimitive> void bindSpoofaxPrimitive(Class<T> primitive) {
        bind(primitive).in(Singleton.class);
        spoofaxPrimitiveLibrary.addBinding().to(primitive);
    }

    protected <T extends AbstractPrimitive & AutoCloseable> void bindAutoClosableSpoofaxPrimitive(Class<T> primitive) {
        bindSpoofaxPrimitive(primitive);
        autoClosableBinder.addBinding().to(primitive);
    }

    protected <T extends AbstractPrimitive> void bindLegacySpoofaxPrimitive(Class<T> primitive) {
        bind(primitive).in(Singleton.class);
        legacySpoofaxPrimitiveLibrary.addBinding().to(primitive);
    }

    protected <T extends AbstractPrimitive & AutoCloseable> void bindAutoClosableLegacySpoofaxPrimitive(Class<T> primitive) {
        bindLegacySpoofaxPrimitive(primitive);
        autoClosableBinder.addBinding().to(primitive);
    }
}
