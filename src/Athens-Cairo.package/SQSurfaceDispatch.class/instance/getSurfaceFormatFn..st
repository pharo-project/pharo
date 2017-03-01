accessing structure variables
getSurfaceFormatFn: anObject
	"This method was automatically generated"
	handle pointerAt: OFFSET_GETSURFACEFORMATFN put: (ExternalAddress fromAddress: anObject thunk address)