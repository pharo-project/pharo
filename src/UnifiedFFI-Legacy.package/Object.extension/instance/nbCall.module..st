*UnifiedFFI-Legacy
nbCall: fnSpec module: aModuleNameOrHandle
	" you can override this method if you need to"
	^ (self ffiCalloutIn: thisContext sender)
		convention: self ffiCallingConvention;
		function: fnSpec module: aModuleNameOrHandle