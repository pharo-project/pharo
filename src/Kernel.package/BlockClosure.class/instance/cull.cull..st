evaluating
cull: firstArg cull: secondArg
	"Execute the receiver with one or two arguments depending on the receiver"
	"([:x | x + 1] cull: 13 cull: 12)
	>>> 14
	"
	"([:x :y | x + y] cull: 3 cull: 2)
	>>> 5
	"
	^numArgs < 2 
		ifTrue: [self cull: firstArg]
		ifFalse: [self value: firstArg value: secondArg]
	