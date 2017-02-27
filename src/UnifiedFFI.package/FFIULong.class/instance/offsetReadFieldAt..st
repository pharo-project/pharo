emitting code
offsetReadFieldAt: offsetVariableName
	self isPointer ifTrue: [ 
		^ self offsetPointerReadFieldAt: offsetVariableName ].
	
	^ String streamContents: [ :stream |
		stream << '^handle ' << self emitSelector << ': ' << offsetVariableName ]