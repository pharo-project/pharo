accessing structure variables
unlockSurfaceFn: aCallbackOrNil
	"This method was automatically generated"
	unlockSurfaceFn := aCallbackOrNil.
	handle 
		pointerAt: OFFSET_UNLOCKSURFACEFN 
		put: (aCallbackOrNil 
			ifNotNil: [ ExternalAddress fromAddress: aCallbackOrNil thunk address ]
			ifNil: [ ExternalAddress null ])