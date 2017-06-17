libgit-fields
prim_certificate_check: anObject
	"This method was automatically generated"
	handle 
		pointerAt: OFFSET_PRIM_CERTIFICATE_CHECK
		put: (anObject 
			ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]
			ifNil: [ ExternalAddress null ])