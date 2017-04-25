accessing structure variables
lockSurfaceFn: aCallback
	"This method was automatically generated"
	lockSurfaceFn := aCallback.
	handle 
		pointerAt: OFFSET_LOCKSURFACEFN 
		put: (ExternalAddress fromAddress: aCallback thunk address)