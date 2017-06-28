emitting code
writeFieldAt: byteOffset with: valueName
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			writeFieldAt: byteOffset
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle platformUnsignedLongAt: ' << byteOffset asString
			<< ' put: ' << valueName ]
