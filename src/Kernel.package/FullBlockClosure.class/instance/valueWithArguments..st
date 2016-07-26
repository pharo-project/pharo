evaluating
valueWithArguments: anArray
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments in an anArray and copied values to
	 the activation as its arguments and copied temps. Primitive. Essential."
	<primitive: 208>
	| newContext |
	numArgs ~= anArray size ifTrue:
		[self numArgsError: anArray size].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			1 to: numArgs do:
				[:i| newContext at: i put: (anArray at: i)].
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]