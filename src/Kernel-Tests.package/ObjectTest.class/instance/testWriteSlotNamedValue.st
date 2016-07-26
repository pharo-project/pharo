tests - introspection
testWriteSlotNamedValue
	| object |
	object := 5@6.
	
	self assert: (object writeSlotNamed: #x value: 7) = 7.
	self assert: object = (7@6).