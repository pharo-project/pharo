tests - fixture
test0FixtureOccurrencesTest
	| tmp |
	self empty.
	self assert: self empty isEmpty.
	self collectionWithoutEqualElements.
	self deny: self collectionWithoutEqualElements isEmpty.
	tmp := OrderedCollection new.
	self collectionWithoutEqualElements
		do: [ :each | 
			self deny: (tmp includes: each).
			tmp add: each ].
	self elementNotInForOccurrences.
	self deny: (self collectionWithoutEqualElements includes: self elementNotInForOccurrences)