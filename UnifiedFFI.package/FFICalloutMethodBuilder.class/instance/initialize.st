initialization
initialize
	super initialize.
	functionResolutionStrategies := FFIFunctionResolutionStrategy allSubclasses collect: #new
		