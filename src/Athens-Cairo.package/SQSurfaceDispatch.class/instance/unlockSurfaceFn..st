accessing structure variables
unlockSurfaceFn: aCallback
	"This method was automatically generated"
	unlockSurfaceFn := aCallback.
	handle 
		pointerAt: OFFSET_UNLOCKSURFACEFN 
		put: (ExternalAddress fromAddress: aCallback thunk address)