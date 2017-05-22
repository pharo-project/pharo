ffi-calls
call: fnSpec
	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		function: fnSpec module: self ffiLibrary