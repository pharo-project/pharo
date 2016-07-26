meta-object-protocol
write: aValue to: anObject

	(super read: anObject) value: aValue.
	^ aValue