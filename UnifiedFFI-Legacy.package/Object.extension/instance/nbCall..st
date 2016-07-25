*UnifiedFFI-Legacy
nbCall: fnSpec
	" you can override this method if you need to"
	
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: self ffiLibraryName