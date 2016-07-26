evaluating
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg
	"Execute the receiver with four or less arguments. Check cull:cull: for examples"
	
	^numArgs < 4 
		ifTrue: [self cull: firstArg cull: secondArg cull: thirdArg]
		ifFalse: [self value: firstArg value: secondArg value: thirdArg value: fourthArg]
	