tests
testSingletonMaterializationEnforced
	| singleton materialized |
	singleton := FLSingletonMockEnforced instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMockEnforced instance.
	
	materialized := self resultOfSerializeAndMaterialize: singleton.
	self assert: materialized == singleton.
	self deny: materialized reference isNil.
	self assert: materialized reference == singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"