tests
testSingletonMaterializationEnforcedNoInstance
	| singleton materialized |
	singleton := FLSingletonMockEnforced instance.
	singleton reference: 'a reference'.
	self assert: singleton equals: FLSingletonMockEnforced instance.
	
	self serialize: singleton.
	FLSingletonMock reset.
	
	materialized := self materialized.
	self deny: materialized == singleton.
	self deny: materialized reference isNil.
	self assert: materialized reference equals: singleton reference.
	
	FLSingletonMock reset "cannot do this in tearDown because that's used by some helper methods"