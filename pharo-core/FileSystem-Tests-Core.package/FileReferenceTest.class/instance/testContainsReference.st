tests
testContainsReference
	| ref |
	ref := filesystem * 'griffle'.
	self assert: (ref contains: ref / 'nurp')