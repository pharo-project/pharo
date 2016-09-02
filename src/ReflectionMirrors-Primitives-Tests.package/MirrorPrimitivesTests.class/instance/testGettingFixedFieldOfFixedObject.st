tests-fields accessing
testGettingFixedFieldOfFixedObject

	| actual  |
	
	actual := MirrorPrimitives fixedFieldOf: 10@20 at: 1.	
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives fixedFieldOf: 10@20 at: 2.	
	self assert: actual equals: 20.
	
	self should: [MirrorPrimitives fixedFieldOf: 10@20 at: 3] raise: PrimitiveFailed