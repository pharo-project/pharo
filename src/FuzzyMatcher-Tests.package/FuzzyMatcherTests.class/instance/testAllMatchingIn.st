tests
testAllMatchingIn

	| input output |
	
	input := #('0g' 'ogb' 'gat' 'dog' 'FroG' 'hog' 'goh' 'hgo').
	output := FuzzyMatcher allMatching: 'og' in: input.
	
	self assert: output asArray equals: #('ogb' 'dog' 'FroG' 'hog')