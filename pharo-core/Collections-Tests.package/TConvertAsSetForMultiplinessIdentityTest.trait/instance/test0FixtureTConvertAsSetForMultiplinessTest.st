tests - fixture
test0FixtureTConvertAsSetForMultiplinessTest
	"a collection  with equal elements:"

	| res |
	self withEqualElements.
	res := true.
	self withEqualElements detect: [ :each | (self withEqualElements occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self assert: res = true