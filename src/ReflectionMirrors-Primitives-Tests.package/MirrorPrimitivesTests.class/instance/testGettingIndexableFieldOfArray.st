tests-fields accessing
testGettingIndexableFieldOfArray
	
	| actual |
	actual := MirrorPrimitives indexableFieldOf: #(10 20) at: 1.
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives indexableFieldOf: #(110 20) at: 2.
	self assert: actual equals: 20.
	
	self should: [MirrorPrimitives indexableFieldOf: #(10 20) at: 3] raise: SubscriptOutOfBounds