*UnifiedFFI
ffiCall: fnSpec module: aModuleName
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: aModuleName 