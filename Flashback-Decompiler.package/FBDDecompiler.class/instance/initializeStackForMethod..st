initialize
initializeStackForMethod: aCompiledMethod
	argCount := 0.
	tempCount := 0.
	self
		initializeStackNumArgs: aCompiledMethod numArgs
		copied: #()
		numTemps: aCompiledMethod numTemps - aCompiledMethod numArgs