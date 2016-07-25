testing
refersToSymbol: aSymbol 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher
		matches: aSymbol printString do: [:node :answer | true];
		matches: '`#literal'
			do: [:node :answer | answer or: [self literal: node value containsReferenceTo: aSymbol]].
	(RBScanner isSelector: aSymbol) 
		ifTrue: 
			[searcher 
				matches: '`@object ' , (RBParseTreeSearcher buildSelectorString: aSymbol)
				do: [:node :answer | true]].
	^searcher executeTree: self parseTree initialAnswer: false