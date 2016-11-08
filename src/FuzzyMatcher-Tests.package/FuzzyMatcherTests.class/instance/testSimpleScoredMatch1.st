tests
testSimpleScoredMatch1
	| col fuzzy matches |
	col := #('a' 'ab' 'abc' 'whj' 'cab' 'cat' 'dog').
	matches := OrderedCollection new.
	fuzzy := FuzzyMatcher pattern: 'a'.
	col
		do:
			[ :each | fuzzy match: each ifScored: [ :score | matches add: each ] ].
	self assert: matches asArray equals: #('a' 'ab' 'abc' 'cab' 'cat')