testing
refersToClassNamed: aSymbol 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: aSymbol asString do: [:node :answer | true].
	^(searcher executeTree: self parseTree initialAnswer: false) 
		or: [self refersToSymbol: aSymbol]