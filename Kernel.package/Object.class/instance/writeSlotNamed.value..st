introspection
writeSlotNamed: aName value: anObject
	^(self class slotNamed: aName) write: anObject to: self