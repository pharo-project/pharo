tests
testPrimitives
	| primitives |
	primitives := STON listClass withAll: { true. false. nil }.
	self serializeAndMaterialize: primitives