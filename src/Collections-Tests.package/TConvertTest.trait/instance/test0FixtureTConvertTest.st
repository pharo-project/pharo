tests - fixture
test0FixtureTConvertTest
	"a collection of number without equal elements:"

	| res |
	self collectionWithoutEqualElements.
	res := true.
	self collectionWithoutEqualElements
		detect: [ :each | (self collectionWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false