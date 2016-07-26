accessing
target: aMorph

	self setTarget: aMorph.
	target ifNotNil: [self addHandles].
