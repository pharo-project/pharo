serialize/materialize
serializeInstance: anObject with: anEncoder
	
	self serializeGlobalAssociationKeyed: (anEncoder globalEnvironment keyAtValue: anObject) on: anEncoder