tests - slots
testSlotNamed
	self assert: ((Point slotNamed: #x) name = #x).
