meta-object-protocol
write: aValue to: anObject

	(super read: anObject) at: 1 put: aValue.
	^ aValue