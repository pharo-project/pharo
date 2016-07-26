accessing
methods
	^nodes flatCollect: [ :entity |
		(entity isKindOf: RBProgramNode) 
			ifTrue: [ {entity methodNode compiledMethod}  ]
			ifFalse: [ entity usingMethods ] ]