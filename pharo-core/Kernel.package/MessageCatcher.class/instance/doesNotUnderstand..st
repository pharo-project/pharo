reflective operations
doesNotUnderstand: aMessage

	accumulator ifNotNil: [accumulator add: aMessage].
	^ aMessage