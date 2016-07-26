accessing
pointer
	"Answer a pointer to the receiver in the form of an Alien.
	 This will fail if the receiver is not indirect."
	self isPointer ifTrue:
		[self error: 'One cannot take the address of pointer Alien instances; they may move, and are already pointers'].
	^self class forPointer: self address