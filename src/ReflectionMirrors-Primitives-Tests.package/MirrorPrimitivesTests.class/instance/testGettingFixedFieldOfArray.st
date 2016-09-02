tests-fields accessing
testGettingFixedFieldOfArray
	
	self should: [MirrorPrimitives fixedFieldOf: #(1 2) at: 1] raise: PrimitiveFailed