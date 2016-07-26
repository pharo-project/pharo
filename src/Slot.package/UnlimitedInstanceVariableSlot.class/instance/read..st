meta-object-protocol
read: anObject
	^(baseSlot read: anObject) at: offset
		