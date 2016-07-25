tests - iterating
testReject

	| res element |
	res := self collectionWithoutNilElements reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  reject: [:each | each = element].
	self assert: res size = (self collectionWithoutNilElements size - 1).
	
	
	