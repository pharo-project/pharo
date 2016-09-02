tests-fields accessing
testChangingFixedFieldOfArray
	
	self should: [MirrorPrimitives fixedFieldOf: #(1 2) at: 1 put: 10] raise: PrimitiveFailed