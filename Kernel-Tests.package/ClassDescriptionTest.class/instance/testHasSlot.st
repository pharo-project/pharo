tests - slots
testHasSlot
	self assert: (Context hasSlot: (Context slotNamed: #sender)).
	self deny: (Context definesSlot: (Context slotNamed: #sender)).