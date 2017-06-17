ffi-calls
callUnchecked: fnSpec options: options

	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibraryName