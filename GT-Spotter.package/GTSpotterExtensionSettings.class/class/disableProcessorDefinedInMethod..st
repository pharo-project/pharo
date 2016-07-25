settings
disableProcessorDefinedInMethod: aMethod
	self disabledExtensions add: (self internalKeyForMethod: aMethod)