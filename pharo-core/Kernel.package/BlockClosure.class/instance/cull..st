evaluating
cull: anArg
	^numArgs = 0 
		ifTrue: [self value]
		ifFalse: [self value: anArg]
	