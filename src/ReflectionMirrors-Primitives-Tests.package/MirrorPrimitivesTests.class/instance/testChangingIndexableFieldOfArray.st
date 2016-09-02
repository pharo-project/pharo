tests-fields accessing
testChangingIndexableFieldOfArray

	| object |
	object := #(10 20).
	
	MirrorPrimitives indexableFieldOf: object at: 1 put: 100.	
	self assert: object first equals: 100.
	
	MirrorPrimitives indexableFieldOf: object at: 2 put: 200.	
	self assert: object second equals: 200.
	
	self should: [MirrorPrimitives indexableFieldOf: object at: 3 put: 500] raise: SubscriptOutOfBounds 