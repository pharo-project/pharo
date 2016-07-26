evaluating
cull: firstArg cull: secondArg cull: thirdArg
	^numArgs < 3 
		ifTrue: [self cull: firstArg cull: secondArg]
		ifFalse: [self value: firstArg value: secondArg value: thirdArg]
	