settings
enableProcessorDefinedInMethod: aMethod
	self disabledExtensions remove: (self internalKeyForMethod: aMethod)