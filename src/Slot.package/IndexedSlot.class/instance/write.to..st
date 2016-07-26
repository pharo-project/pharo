meta-object-protocol
write: aValue to: anObject
	^ thisContext object: anObject instVarAt: index put: aValue.