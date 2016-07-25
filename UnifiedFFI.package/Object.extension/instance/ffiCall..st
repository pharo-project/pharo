*UnifiedFFI
ffiCall: fnSpec
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: self ffiLibraryName