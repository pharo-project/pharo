meta-object-protocol
read: anObject
	^ (baseSlot read: anObject) at: self name ifAbsent: nil