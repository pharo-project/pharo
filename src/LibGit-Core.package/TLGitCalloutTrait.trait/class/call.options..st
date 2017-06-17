ffi-calls
call: fnSpec options: options
	^ (self ffiCalloutIn: thisContext sender)
		cdecl;
		options: options;
		function: fnSpec module: self ffiLibrary