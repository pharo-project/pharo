evaluating
valueWithPossibleArgument: anArg 
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg, if it requires more than one,
	 fill up the rest with nils."

	| a |
	numArgs = 0 ifTrue: [^self value].
	numArgs = 1 ifTrue: [^self value: anArg].
	a := Array new: numArgs.
	a at: 1 put: anArg.
	^self valueWithArguments: a