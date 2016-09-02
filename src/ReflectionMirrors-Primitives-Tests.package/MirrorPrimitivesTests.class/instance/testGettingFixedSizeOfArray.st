tests-fields accessing
testGettingFixedSizeOfArray

	| actual  |
	
	actual := MirrorPrimitives fixedSizeOf: #(1 2).
	
	self assert: actual equals: 0