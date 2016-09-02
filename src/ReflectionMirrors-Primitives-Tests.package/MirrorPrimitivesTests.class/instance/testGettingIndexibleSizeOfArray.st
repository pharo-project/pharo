tests-fields accessing
testGettingIndexibleSizeOfArray

	| actual |
	
	actual := MirrorPrimitives indexableSizeOf: #(1 2).
	
	self assert: actual equals: 2