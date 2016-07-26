converting
asExternalTypeOn: generator
	^ FFIExternalValueHolderType 
		objectClass: self 
		representationType: self type