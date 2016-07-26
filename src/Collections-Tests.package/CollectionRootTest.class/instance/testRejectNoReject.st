tests - iterate
testRejectNoReject
	| res collection |
	collection := self collectionWithoutNilElements .
	res := collection  reject: [ :each | each isNil ].
	self assert: res size = collection  size