accessing structure variables
showSurfaceFn: aCallbackOrNil
	"This method was automatically generated"
	showSurfaceFn := aCallbackOrNil.
	handle 
		pointerAt: OFFSET_SHOWSURFACEFN 
		put: (aCallbackOrNil 
			ifNotNil: [ ExternalAddress fromAddress: aCallbackOrNil thunk address ]
			ifNil: [ ExternalAddress null ])