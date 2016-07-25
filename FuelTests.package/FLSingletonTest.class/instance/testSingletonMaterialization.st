tests
testSingletonMaterialization
	| singleton materialized |
	singleton := FLSingletonMock instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMock instance.
	
	materialized := self resultOfSerializeAndMaterialize: singleton.
	self deny: materialized == singleton.
	self deny: materialized reference isNil.
	self deny: materialized reference == singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"