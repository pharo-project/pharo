evaluating
cull: firstArg cull: secondArg
	^numArgs < 2 
		ifTrue: [self cull: firstArg]
		ifFalse: [self value: firstArg value: secondArg]
	