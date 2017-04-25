private emitting code
offsetPointerReadFieldAt: offsetVariableName
	^ '^ExternalData 
		fromHandle: (handle {1}: {2})
		type: FFIArchitecture forCurrentArchitecture externalLongType asPointerType'
	format: { 
		self emitSelector.
		offsetVariableName }