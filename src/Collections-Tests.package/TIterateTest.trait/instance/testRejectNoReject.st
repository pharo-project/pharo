tests - iterating
testRejectNoReject

	| res |
	res := self collectionWithoutNilElements  reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	