private emitting code
offsetPointerReadFieldAt: offsetVariableName
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalULongType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }