tests - as set tests
testAsIdentitySetWithEqualsElements
	| result collection |
	collection := self withEqualElements .
	result := collection asIdentitySet.
	collection do: [ :each | self assert: (result occurrencesOf: each) = 1 ].
	self assert: result class = IdentitySet.