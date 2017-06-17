libgit-fields
prim_credentials: anObject
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CREDENTIALS
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])