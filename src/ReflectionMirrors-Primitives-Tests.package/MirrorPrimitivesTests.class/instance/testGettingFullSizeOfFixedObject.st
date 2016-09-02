tests-fields accessing
testGettingFullSizeOfFixedObject

	| actual  |
	
	actual := MirrorPrimitives sizeOf: 0@0.
	
	self assert: actual equals: 2