evaluating
value
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]