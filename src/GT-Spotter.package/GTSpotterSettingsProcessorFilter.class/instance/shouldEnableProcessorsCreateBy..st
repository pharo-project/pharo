testing
shouldEnableProcessorsCreateBy: aMethod
	"Enable everything."
	
	^ (GTSpotterExtensionSettings shouldSpotterIgnoreProcessorDefinedInMethod: aMethod) not