evaluating
value: anArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this message.
	 Supply the argument and copied values to the activation as its arguments and copied temps.
	 Primitive. Optional (but you're going to want this for performance)."
	| newContext ncv |
	<primitive: 202>
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed