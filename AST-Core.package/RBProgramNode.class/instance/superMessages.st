querying
superMessages
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: 'super `@msg: ``@args'
		do: 
			[:aNode :answer | 
			answer
				add: aNode selector;
				yourself].
	^searcher executeTree: self initialAnswer: Set new