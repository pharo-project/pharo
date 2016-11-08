tests
testSimpleScoredMatch2
	| col fuzzy matches |
	col := #('0g' 'ogb' 'gat' 'dog' 'frog' 'hog' 'goh' 'hgo').
	matches := OrderedCollection new.
	fuzzy := FuzzyMatcher pattern: 'og'.
	col
		do:
			[ :each | fuzzy match: each ifScored: [ :score | matches add: each ] ].
	self assert: matches asArray equals: #('ogb' 'dog' 'frog' 'hog')