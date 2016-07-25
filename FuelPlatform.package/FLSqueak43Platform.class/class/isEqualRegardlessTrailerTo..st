private-extensions-compiledmethod
isEqualRegardlessTrailerTo: aCompiledMethod
	<flExtensionOf: #CompiledMethod>
	^ (self copyWithTrailerBytes: CompiledMethodTrailer empty) fuelEquals: (aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty)