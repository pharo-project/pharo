meta-object-protocol
read: anObject
	^((self baseSlotRead: anObject) bitAt: offset) == 1