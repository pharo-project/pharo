instance creation
outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil
	^(self new: anArrayOrNil basicSize)
		outerContext: aContext
		startpc: aStartpc
		numArgs: argCount
		copiedValues: anArrayOrNil