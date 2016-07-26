*FuelTests
isEqualRegardlessTrailerTo: aCompiledMethod
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) = (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)