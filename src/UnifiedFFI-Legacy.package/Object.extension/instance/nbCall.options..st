*UnifiedFFI-Legacy
nbCall: fnSpec options: callOptions
	" you can override this method if you need to"

	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: self ffiLibraryName