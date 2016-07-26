tests - fixture
test0FixtureTConvertAsSetForMultiplinessTest
	"a collection ofFloat with equal elements:"

	| res |
	self withEqualElements.
	self withEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self withEqualElements detect: [ :each | (self withEqualElements occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self assert: res = true.	"a collection of Float without equal elements:"
	self elementsCopyNonIdenticalWithoutEqualElements.
	self elementsCopyNonIdenticalWithoutEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self elementsCopyNonIdenticalWithoutEqualElements
		detect: [ :each | (self elementsCopyNonIdenticalWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false