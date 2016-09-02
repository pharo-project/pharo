tests-fields accessing
testGettingIndexibleSizeOfFixedObject

	| actual  |
	
	actual := MirrorPrimitives indexableSizeOf: 0@0.
	
	self assert: actual equals: 0