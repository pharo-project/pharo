accessing structure variables
getSurfaceFormatFn: aCallback
	"This method was automatically generated"
	getSurfaceFormatFn := aCallback.
	handle 
		pointerAt: OFFSET_GETSURFACEFORMATFN 
		put: (ExternalAddress fromAddress: aCallback thunk address)