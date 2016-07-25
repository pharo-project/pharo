*UnifiedFFI
ffiCall: fnSpec module: aModuleName options: callOptions
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		options: callOptions;
		function: fnSpec module: aModuleName 