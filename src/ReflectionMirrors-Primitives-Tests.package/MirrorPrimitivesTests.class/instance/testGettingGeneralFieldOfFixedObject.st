tests-fields accessing
testGettingGeneralFieldOfFixedObject

	| actual  |
	
	actual := MirrorPrimitives fieldOf: 10@20 at: 1.	
	self assert: actual equals: 10.
	
	actual := MirrorPrimitives fieldOf: 10@20 at: 2.	
	self assert: actual equals: 20.
	
	self should: [MirrorPrimitives fieldOf: 10@20 at: 3] raise: PrimitiveFailed