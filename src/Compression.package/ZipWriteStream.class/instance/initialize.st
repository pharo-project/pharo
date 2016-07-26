initialization
initialize
	super initialize.
	literals := ByteArray new: WindowSize.
	distances := WordArray new: WindowSize.
	literalFreq := WordArray new: MaxLiteralCodes.
	distanceFreq := WordArray new: MaxDistCodes.
	self initializeNewBlock.
