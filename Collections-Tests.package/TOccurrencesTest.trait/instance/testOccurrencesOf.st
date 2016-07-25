tests - occurrencesOf
testOccurrencesOf
	| collection |
	collection := self collectionWithoutEqualElements .
	
	collection do: [ :each | self assert: (collection occurrencesOf: each) = 1 ].