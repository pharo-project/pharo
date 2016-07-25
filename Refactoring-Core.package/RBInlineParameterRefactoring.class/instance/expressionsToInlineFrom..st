private
expressionsToInlineFrom: aTree 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: '``@obj ' , (self buildSelectorString: oldSelector)
		do: 
			[:aNode :answer | 
			answer
				add: (aNode arguments at: parameterIndex);
				yourself].
	^searcher executeTree: aTree initialAnswer: OrderedCollection new