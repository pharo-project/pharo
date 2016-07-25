private
variableRead: aNode 
	(self currentScope includesKey: aNode name) 
		ifTrue: 
			[(self currentScope at: aNode name) isNil 
				ifTrue: [self currentScope at: aNode name put: true]]