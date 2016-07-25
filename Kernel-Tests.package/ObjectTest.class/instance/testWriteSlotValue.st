tests - introspection
testWriteSlotValue
	| object |
	object := 5@6.
	
	self assert: (object writeSlot: (Point slotNamed: #x) value: 7) = 7.
	self assert: object = (7@6).