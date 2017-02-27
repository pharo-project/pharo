emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	self isPointer ifTrue: [ 
		^ self externalTypeWithArity 
			offsetWriteFieldAt: offsetVariableName 
			with: valueName ].

	^ String streamContents: [ :stream |
		stream 
			<< '^handle ' << self emitSelector << ': ' << offsetVariableName 
			<< ' put: ' << valueName ]