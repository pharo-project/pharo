tests
testBasic

	| result |
	self deny: FLGlobalSendMock newInstanceToSerialize = FLGlobalSendMock newInstanceToMaterialize.

	result := self resultOfSerializeAndMaterialize: FLGlobalSendMock newInstanceToSerialize.
	self assert: FLGlobalSendMock newInstanceToMaterialize = result.