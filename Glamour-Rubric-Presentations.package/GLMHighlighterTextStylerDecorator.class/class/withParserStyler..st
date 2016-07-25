instance creation
withParserStyler: aParser
	
	^ self new styler: (GLMHighlighterTextParserStyler new parser: aParser)