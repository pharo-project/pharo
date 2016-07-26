tests - fixture
test0FixtureAsSetForIdentityMultiplinessTest
	"a collection (of elements for which copy is not identical ) without equal elements:"

	| anElement res |
	self elementsCopyNonIdenticalWithoutEqualElements.
	anElement := self elementsCopyNonIdenticalWithoutEqualElements anyOne.
	self deny: anElement copy == anElement.
	res := true.
	self elementsCopyNonIdenticalWithoutEqualElements
		detect: [ :each | (self elementsCopyNonIdenticalWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false