meta-object-protocol
write: aValue to: anObject
	^ (baseSlot read: anObject) at: self name put: aValue