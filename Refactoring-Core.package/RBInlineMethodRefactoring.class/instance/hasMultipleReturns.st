testing
hasMultipleReturns
	"Do we have multiple returns? If the last statement isn't a return, then we have an implicit return of self."

	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: '^``@object'
		do: 
			[:aNode :hasAReturn | 
			hasAReturn ifTrue: [^true].
			true].
	searcher executeTree: inlineParseTree
		initialAnswer: inlineParseTree lastIsReturn not.
	^false