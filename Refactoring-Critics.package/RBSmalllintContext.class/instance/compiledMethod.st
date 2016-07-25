accessing
compiledMethod
	^compiledMethod notNil
		ifTrue: [compiledMethod]
		ifFalse: [compiledMethod := class compiledMethodAt: selector]