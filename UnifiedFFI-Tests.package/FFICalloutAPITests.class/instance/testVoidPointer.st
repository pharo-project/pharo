tests pointer
testVoidPointer
	| result dest |
	
	result := self 
		primMemMoveFrom: 'Hello, World' asByteArray 
		to: (dest := ByteArray new: 12) 
		size: 12.
		
	self assert: dest asString = 'Hello, World'