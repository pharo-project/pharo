settings
shouldSpotterIgnoreProcessorDefinedInMethod: aCompiledMethod
	^ self disabledExtensions includes: aCompiledMethod methodClass name, '_', aCompiledMethod selector allButLast