tests - fixture
test0FixtureOccurrencesForMultiplinessTest
	| cpt anElement collection |
	self collectionWithEqualElements.
	self collectionWithEqualElements.
	self elementTwiceInForOccurrences.
	anElement := self elementTwiceInForOccurrences.
	collection := self collectionWithEqualElements.
	cpt := 0.	" testing with identity check ( == ) so that identy collections can use this trait : "
	self collectionWithEqualElements
		do: [ :each | 
			each == self elementTwiceInForOccurrences
				ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 2