configuring
setTrailerWithNoChange
	"Make compiled methods be serialized as they are."

	^ self transformationForSerializing: [:aCompiledMethod | aCompiledMethod ]