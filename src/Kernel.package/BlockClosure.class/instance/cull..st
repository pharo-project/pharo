evaluating
cull: anArg
	"Execute the receiver with one or zero arguments depending on the receiver"
	"([ 12 ] cull: 13)
	>>> 12 
	"
	"([:x | x + 12] cull: 3)
	>>> 15
	"
	
	^numArgs = 0 
		ifTrue: [self value]
		ifFalse: [self value: anArg]
	