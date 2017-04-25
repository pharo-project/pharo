accessing structure variables
showSurfaceFn: aCallback
	"This method was automatically generated"
	showSurfaceFn := aCallback.
	handle 
		pointerAt: OFFSET_SHOWSURFACEFN 
		put: (ExternalAddress fromAddress: aCallback thunk address)