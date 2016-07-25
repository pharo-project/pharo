serialize/materialize
serializeInstance: anObject with: anEncoder
	
	| globalSend |
	globalSend := globalSends at: anObject.
	
	self serializeGlobalAssociationKeyed: globalSend key on: anEncoder.
	anEncoder encodeString: globalSend value.
