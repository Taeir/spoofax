package org.strategoxt.imp.runtime.parser;

import java.io.IOException;
import java.io.InputStream;

import lpg.runtime.IPrsStream;

import org.spoofax.jsglr.BadTokenException;
import org.spoofax.jsglr.Disambiguator;
import org.spoofax.jsglr.FilterException;
import org.spoofax.jsglr.NoRecoveryRulesException;
import org.spoofax.jsglr.ParseTable;
import org.spoofax.jsglr.SGLR;
import org.spoofax.jsglr.SGLRException;
import org.spoofax.jsglr.TokenExpectedException;
import org.strategoxt.imp.runtime.Environment;
import org.strategoxt.imp.runtime.parser.tokens.TokenKindManager;

import aterm.ATerm;

/**
 * IMP IParser implementation using JSGLR, imploding parse trees to AST nodes and tokens.
 *
 * @author Lennart Kats <L.C.L.Kats add tudelft.nl>
 */ 
public class JSGLRI extends AbstractSGLRI {
	
	private ParseTable parseTable;
	
	private boolean useRecovery = false;
	
	private SGLR parser;
	
	private Disambiguator disambiguator;
	
	// Initialization and parsing
	
	public JSGLRI(ParseTable parseTable, String startSymbol,
			SGLRParseController controller, TokenKindManager tokenManager) {
		super(controller, tokenManager, startSymbol, parseTable);
		
		this.parseTable = parseTable;
		resetState();
	}
	
	public JSGLRI(ParseTable parseTable, String startSymbol) {
		this(parseTable, startSymbol, null, new TokenKindManager());
	}
	
	public void asyncAbort() {
		parser.asyncAbort();
	}
	
	protected SGLR getParser() {
		return parser;
	}
	
	/**
	 * @see SGLR#setUseStructureRecovery(boolean)
	 */
	public void setUseRecovery(boolean useRecovery) throws NoRecoveryRulesException {
		this.useRecovery = useRecovery;
		parser.setUseStructureRecovery(useRecovery);
	}
	
	public ParseTable getParseTable() {
		return parseTable;
	}
	
	public Disambiguator getDisambiguator() {
		return disambiguator;
	}
	
	public IPrsStream getIPrsStream() {
		return super.getController().getIPrsStream();
	}
	
	public void setParseTable(ParseTable parseTable) {
		this.parseTable = parseTable;
		resetState();
	}
	
	@Override
	protected ATerm doParseNoImplode(char[] inputChars, String filename)
			throws TokenExpectedException, BadTokenException, SGLRException, IOException {
		
		return doParseNoImplode(toByteStream(inputChars), inputChars);
	}
	
	/**
	 * Resets the state of this parser, reinitializing the SGLR instance
	 */
	void resetState() {
		parser = Environment.createSGLR(parseTable);
		if (disambiguator != null) parser.setDisambiguator(disambiguator);
		else disambiguator = parser.getDisambiguator();
		try {
			setUseRecovery(useRecovery);
		} catch (NoRecoveryRulesException e) {
			// Already handled/logged this error in setRecoverHandler()
		}
	}
	
	private ATerm doParseNoImplode(InputStream inputStream, char[] inputChars)
			throws TokenExpectedException, BadTokenException, SGLRException, IOException {
		
		// FIXME: Some bug in JSGLR is causing its state to get corrupted; must reset it every parse
		resetState();
		
		// Read stream using tokenizer/lexstream
		
		try {
			return parser.parse(inputStream, getStartSymbol());
		} catch (FilterException e) {
			if (parser.getDisambiguator().getFilterPriorities()) {
				Environment.logException("Parse filter failure - disabling priority filters and trying again", e);
				getDisambiguator().setFilterPriorities(false);
				try {
					return parser.parse(inputStream, getStartSymbol());
				} finally {
					getDisambiguator().setFilterPriorities(true);
				}
			} else {
				throw new FilterException(e.getParser(), e.getMessage(), e);
			}
		}
	}
}
