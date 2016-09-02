tests-fields accessing
testGettingFixedSizeOfFixedObject

	| actual  |
	
	actual := MirrorPrimitives fixedSizeOf: 0@0.
	
	self assert: actual equals: 2