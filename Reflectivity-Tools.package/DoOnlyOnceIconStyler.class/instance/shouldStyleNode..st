testing
shouldStyleNode: aNode
	^aNode isMessage and: [ aNode selector = #doOnlyOnce: ]