tests-fields accessing
testChangingFixedFieldOfFixedObject

	| object |
	object := 10@20.
	
	MirrorPrimitives fixedFieldOf: object at: 1 put: 100.	
	self assert: object x equals: 100.
	
	MirrorPrimitives fixedFieldOf: object at: 2 put: 200.	
	self assert: object y equals: 200.
	
	self should: [MirrorPrimitives fixedFieldOf: object at: 3 put: 500] raise: PrimitiveFailed