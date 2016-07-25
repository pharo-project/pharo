querying
selfMessages
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: 'self `@msg: ``@args'
		do: 
			[:aNode :answer | 
			answer
				add: aNode selector;
				yourself].
	^searcher executeTree: self initialAnswer: Set new