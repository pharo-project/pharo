meta-object-protocol
write: aValue to: anObject
		| newInteger |
		newInteger := (self baseSlotRead: anObject) bitAt: offset put: aValue asBit.
		baseSlot write: newInteger to: anObject.
		^aValue