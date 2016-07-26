tests - slots
testHasSlotNamed
	self assert: (Context hasSlotNamed: #sender).
	self assert: (Context hasSlotNamed: #method).