selectors
computeMessagesSentToVariables
	| searcher |
	variableMessages := Dictionary new.
	class instanceVariableNames 
		do: [:each | variableMessages at: each put: Set new].
	searcher := RBParseTreeSearcher new.
	class instanceVariableNames do: 
			[:each | 
			| block |
			block := [:aNode :answer | 
				(variableMessages at: each ifAbsentPut: [Set new]) add: aNode selector.
				self processCollectionMessagesFor: each in: aNode].
			searcher matches: each , ' `@messageName: ``@args' do: block.
			(backpointers at: each) do: 
					[:sel | 
					sel isSymbol 
						ifTrue: 
							[searcher 
								matches: ('(self <1s>) `@messageName: ``@args' 
										expandMacrosWith: (RBParseTreeSearcher buildSelectorString: sel)) asString
								do: block]]].
	searcher answer: variableMessages.
	self executeSearch: searcher