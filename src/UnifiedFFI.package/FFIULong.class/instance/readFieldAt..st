emitting code
readFieldAt: byteOffset
	self isPointer ifTrue: [ 
		^ self pointerReadFieldAt: byteOffset ].

	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << byteOffset asString ].
