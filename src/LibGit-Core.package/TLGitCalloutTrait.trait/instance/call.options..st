ffi-calls
call: fnSpec options: options
	^ (self safeFFICalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibraryName