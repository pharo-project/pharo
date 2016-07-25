selectors-collections
processCollectionFor: key messagesTo: aName in: aBlock 
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher 
		matches: aName , ' `@message: ``@args'
		do: [ :aNode :answer | 
			self processCollectionMessagesFor: key in: aNode.
			answer add: aNode selector; yourself ].
	searcher 
		executeTree: aBlock
		initialAnswer: (variableMessages at: (self collectionNameFor: key) ifAbsentPut: [ Set new ])