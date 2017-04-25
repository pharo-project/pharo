emitting code
writeFieldAt: byteOffset with: valueName
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			writeFieldAt: byteOffset
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << byteOffset asString
			<< ' put: ' << valueName ]
