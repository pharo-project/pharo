tests - introspection
testReadSlot
	self assert: (5@3 readSlot: (Point slotNamed: #x)) = 5