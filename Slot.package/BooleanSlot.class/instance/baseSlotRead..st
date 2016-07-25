accessing
baseSlotRead: anObject
	^ (baseSlot read: anObject) ifNil: [ 0 ].
	