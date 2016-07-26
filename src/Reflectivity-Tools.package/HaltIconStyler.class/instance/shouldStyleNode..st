testing
shouldStyleNode: aNode
	^aNode isMessage and: [ aNode selector = 'halt']