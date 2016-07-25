event handler
aClassHasBeenCommented: anEvent

	anEvent newStamp ifNil: [ ^self ].
	self addMethodReference: (self createAClassReference: anEvent)