accessing
slot: aSlot

	self
		assert: [ aSlot isNotNil ]
		description: [ 'Slot can not be nil' ].

	slot := aSlot.
	value := self hostObject readSlot: slot