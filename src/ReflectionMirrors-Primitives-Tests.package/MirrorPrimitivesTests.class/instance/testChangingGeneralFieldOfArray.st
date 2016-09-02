tests-fields accessing
testChangingGeneralFieldOfArray

	| object |
	object := #(10 20).
	
	MirrorPrimitives fieldOf: object at: 1 put: 100.	
	self assert: object first equals: 100.
	
	MirrorPrimitives fieldOf: object at: 2 put: 200.	
	self assert: object second equals: 200.
	
	self should: [MirrorPrimitives fieldOf: object at: 3 put: 500] raise: SubscriptOutOfBounds  