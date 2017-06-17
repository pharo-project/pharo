converting
asExternalTypeOn: generator
	^ FFIExternalEnumerationType 
		objectClass: self 
		representationType: self representationType