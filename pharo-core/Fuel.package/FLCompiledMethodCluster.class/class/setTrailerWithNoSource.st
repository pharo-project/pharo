configuring
setTrailerWithNoSource
	"Make compiled methods be serialized without source code."

	^ self transformationForSerializing: [:aCompiledMethod |
		aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty ]