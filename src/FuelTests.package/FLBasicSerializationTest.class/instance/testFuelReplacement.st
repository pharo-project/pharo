tests-graph-modification
testFuelReplacement
	| object |
	self 
		assert: (self resultOfSerializeAndMaterialize: FLReplacementClassMock)
		equals: nil.
		
	self
		assert: (self resultOfSerializeAndMaterialize: {FLReplacementClassMock})
		equals: { nil }.

	object := FLReplacementMock new
		ignoreMe;
		yourself.
	self 
		assert: (self resultOfSerializeAndMaterialize: {object})
		equals: { nil }.
		
	object dontIgnoreMe.
	self 
		assert: (self resultOfSerializeAndMaterialize: {object})
		equals: { object }