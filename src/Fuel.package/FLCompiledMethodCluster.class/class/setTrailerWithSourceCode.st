configuring
setTrailerWithSourceCode
	"Make compiled methods be serialized with embedded source code."

	^ self transformationForSerializing: [:aCompiledMethod | 
		aCompiledMethod copyWithTrailerBytes: 
			(CompiledMethodTrailer new sourceCode: aCompiledMethod sourceCode) ]