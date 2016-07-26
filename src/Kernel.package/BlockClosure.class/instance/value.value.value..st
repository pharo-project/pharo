evaluating
value: firstArg value: secondArg value: thirdArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this message.
	 Supply the arguments and copied values to the activation as its arguments and copied temps.
	 Primitive. Optional (but you're going to want this for performance)."
	| newContext ncv |
	<primitive: 204>
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	^self primitiveFailed