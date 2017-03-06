settings
shouldSpotterIgnoreProcessorDefinedInMethod: aCompiledMethod
	"
	Return a boolean value indicating if the spotter extension method, provided as argument, has to be disabled or not
	 
	self shouldSpotterIgnoreProcessorDefinedInMethod: (MCPackage>> #spotterForPackageFor:)
	"
	^ self disabledExtensions includes: aCompiledMethod methodClass name, '_', aCompiledMethod selector allButLast