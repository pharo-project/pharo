transforming
usesDirectly: aParseTree 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher
		matches: '[:`@args | | `@temps | `@.Statements]'
			do: [:aNode :answer | answer];
		matches: name do: [:aNode :answer | true].
	^searcher executeTree: aParseTree initialAnswer: false