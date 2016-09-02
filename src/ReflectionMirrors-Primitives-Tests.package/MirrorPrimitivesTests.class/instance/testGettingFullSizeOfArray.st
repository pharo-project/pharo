tests-fields accessing
testGettingFullSizeOfArray

	| actual |
	
	actual := MirrorPrimitives sizeOf: #(1 2).
	
	self assert: actual equals: 2