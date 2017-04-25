accessing structure variables
getSurfaceFormatFn
	"This method was automatically generated"
	^FFICallback forAddress: ((handle pointerAt: OFFSET_GETSURFACEFORMATFN) asInteger)