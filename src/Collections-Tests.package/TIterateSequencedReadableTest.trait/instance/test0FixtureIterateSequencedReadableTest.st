tests - fixture
test0FixtureIterateSequencedReadableTest

	| res |
	
	self nonEmptyMoreThan1Element.
	self assert: self nonEmptyMoreThan1Element  size > 1.
	
	
	self empty.
	self assert: self empty isEmpty .
	
	res := true.
	self nonEmptyMoreThan1Element    
	detect: [ :each | (self nonEmptyMoreThan1Element    occurrencesOf: each) > 1 ]
	ifNone: [ res := false ].
	self assert: res = false.