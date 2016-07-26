*UnifiedFFI
ffiCall: fnSpec options: callOptions
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: self ffiLibraryName