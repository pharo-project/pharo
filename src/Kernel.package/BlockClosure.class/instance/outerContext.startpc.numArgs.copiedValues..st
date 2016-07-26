initialize-release
outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil
	outerContext := aContext.
	startpc := aStartpc.
	numArgs := argCount.
	1 to: self numCopiedValues do:
		[:i|
		self at: i put: (anArrayOrNil at: i)]