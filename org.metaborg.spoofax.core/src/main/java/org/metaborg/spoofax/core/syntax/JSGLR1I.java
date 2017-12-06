package org.metaborg.spoofax.core.syntax;

import java.io.IOException;
import java.util.Set;

import javax.annotation.Nullable;

import org.apache.commons.vfs2.FileObject;
import org.metaborg.core.language.ILanguageImpl;
import org.metaborg.core.messages.IMessage;
import org.metaborg.core.messages.MessageSeverity;
import org.metaborg.core.messages.MessageUtils;
import org.metaborg.spoofax.core.unit.ParseContrib;
import org.metaborg.util.time.Timer;
import org.spoofax.interpreter.terms.IStrategoTerm;
import org.spoofax.interpreter.terms.ITermFactory;
import org.spoofax.jsglr.client.Asfix2TreeBuilder;
import org.spoofax.jsglr.client.Disambiguator;
import org.spoofax.jsglr.client.FilterException;
import org.spoofax.jsglr.client.InvalidParseTableException;
import org.spoofax.jsglr.client.NullTreeBuilder;
import org.spoofax.jsglr.client.ParseTable;
import org.spoofax.jsglr.client.SGLRParseResult;
import org.spoofax.jsglr.client.StartSymbolException;
import org.spoofax.jsglr.client.imploder.NullTokenizer;
import org.spoofax.jsglr.client.imploder.TermTreeFactory;
import org.spoofax.jsglr.client.imploder.TreeBuilder;
import org.spoofax.jsglr.io.SGLR;
import org.spoofax.jsglr.shared.BadTokenException;
import org.spoofax.jsglr.shared.SGLRException;
import org.spoofax.terms.attachments.ParentTermFactory;

public class JSGLR1I extends JSGLRI<ParseTable> {
    private final SGLR parser;
    
    public JSGLR1I(IParserConfig config, ITermFactory termFactory, ILanguageImpl language, ILanguageImpl dialect,
        @Nullable FileObject resource, String input) throws IOException, InvalidParseTableException {
        super(config, termFactory, language, dialect, resource, input);

        final TermTreeFactory factory = new TermTreeFactory(new ParentTermFactory(termFactory));
        this.parser = new SGLR(new TreeBuilder(factory), getParseTable(config.getParseTableProvider()));
    }

    public ParseContrib parse(@Nullable JSGLRParserConfiguration parserConfig) throws IOException {
        if(parserConfig == null) {
            parserConfig = new JSGLRParserConfiguration();
        }

        final String fileName = resource != null ? resource.getName().getURI() : null;

        final JSGLRParseErrorHandler errorHandler = new JSGLRParseErrorHandler(this, resource,
        		getParseTable(config.getParseTableProvider()).hasRecovers());

        final Timer timer = new Timer(true);
        SGLRParseResult result;
        try {
            // should throw a fatal, or return a non-null result
            result = actuallyParse(input, fileName, parserConfig);
            assert result != null;
        } catch(SGLRException | InterruptedException e) {
            result = null;
            errorHandler.setRecoveryFailed(parserConfig.recovery);
            errorHandler.processFatalException(new NullTokenizer(input, fileName), e);
        }
        final long duration = timer.stop();

        final IStrategoTerm ast;
        if(result != null) {
            // No fatals occurred, so either parsing succeeded or recovery succeeded
            ast = (IStrategoTerm) result.output;
            if(ast == null) {
                // this should only happen if we passed a NullTreeBuilder and parsing succeeded
                // so we have nothing to do
                assert parser.getTreeBuilder() instanceof NullTreeBuilder;
            } else {
                // in case recovery was required, collect the recoverable errors
                errorHandler.setRecoveryFailed(false);
                errorHandler.gatherNonFatalErrors(ast);
                if(resource != null) {
                    SourceAttachment.putSource(ast, resource);
                }
            }
        } else {
            ast = null;
        }

        final boolean hasAst = ast != null;
        final Iterable<IMessage> messages = errorHandler.messages();
        final boolean hasErrors = MessageUtils.containsSeverity(messages, MessageSeverity.ERROR);
        return new ParseContrib(hasAst, hasAst && !hasErrors, ast, messages, duration);
    }

    public SGLRParseResult actuallyParse(String text, @Nullable String filename,
        @Nullable JSGLRParserConfiguration parserConfig) throws SGLRException, InterruptedException {
        if(!parserConfig.implode) {
            // GTODO: copied from existing code. Is this correct? Seems like this should be the tree builder when
            // implode is set to true. Also, there is no else branch.
            parser.setTreeBuilder(new Asfix2TreeBuilder(termFactory));
        }
        parser.setUseStructureRecovery(parserConfig.recovery);
        if(parserConfig.cursorPosition == Integer.MAX_VALUE) {
            parser.setCompletionParse(false, Integer.MAX_VALUE);
        } else {
            parser.setCompletionParse(parserConfig.completion, parserConfig.cursorPosition);
        }
        parser.setTimeout(parserConfig.timeout);

        final Disambiguator disambiguator = parser.getDisambiguator();

        if(dialect != null) {
            disambiguator.setHeuristicFilters(true);
        } else {
            disambiguator.setHeuristicFilters(false);
        }

        String startSymbol = getOrDefaultStartSymbol(parserConfig);
        try {
            return parser.parse(text, filename, startSymbol);
        } catch(FilterException e) {
            if(e.getCause() == null && disambiguator.getFilterPriorities()) {
                disambiguator.setFilterPriorities(false);
                try {
                    return parser.parse(text, filename, startSymbol);
                } finally {
                    disambiguator.setFilterPriorities(true);
                }
            }
            throw e;
        } catch(StartSymbolException e) {
            if(dialect != null) {
                // Parse with all symbols as start symbol when start symbol cannot be found and a dialect is set,
                // indicating that we're parsing Stratego with concrete syntax extensions. We need to parse with all
                // symbols as start symbol, because the start symbol is unknown.
                return parser.parse(text, filename, null);
            } else {
                throw e;
            }
        }
    }
    
    public Set<BadTokenException> getCollectedErrors() {
        return parser.getCollectedErrors();
    }
}
