private
variableWritten: aNode 
	(self currentScope includesKey: aNode variable name) 
		ifTrue: 
			[(self currentScope at: aNode variable name) isNil 
				ifTrue: [self currentScope at: aNode variable name put: false]]