tests - as set tests
testAsSetWithEqualsElements
	| result |
	result := self withEqualElements asSet.
	self withEqualElements do: [ :each | self assert: (result occurrencesOf: each) = 1 ].
	self assert: result class = Set