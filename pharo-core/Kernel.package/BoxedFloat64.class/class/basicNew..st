instance creation
basicNew: sizeRequested 
	"Primitive. Answer an instance of this class with the number
	 of indexable variables specified by the argument, sizeRequested.
	 Fail if this class is not indexable or if the argument is not a
	 positive Integer, or if there is not enough memory available. 
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	sizeRequested isInteger ifTrue:
		[^sizeRequested = 2
			ifTrue: "arg okay; space must be low."
				[OutOfMemory signal.
				 self basicNew: sizeRequested]  "retry if user proceeds"
			ifFalse:
				[self error: 'a Float shall always have two slots']].
	self primitiveFailed