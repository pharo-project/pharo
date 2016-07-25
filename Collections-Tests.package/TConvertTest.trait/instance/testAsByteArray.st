tests - converting
testAsByteArray
	| res |
	self integerCollectionWithoutEqualElements.
	self integerCollectionWithoutEqualElements do: [ :each | self assert: each class = SmallInteger ].
	res := true.
	self integerCollectionWithoutEqualElements
		detect: [ :each | (self integerCollectionWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false.
	self assertSameContents: self integerCollectionWithoutEqualElements whenConvertedTo: ByteArray