ffi-calls
call: fnSpec
	^ (self safeFFICalloutIn: thisContext sender)
		cdecl;
		function: fnSpec module: self ffiLibraryName