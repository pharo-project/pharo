tests
testSortedByScore

	| input output |
	
	input := #('abc' 'a'  'whj' 'cab' 'cat' 'dog' 'ab').	
		
	output := FuzzyMatcher allSortedByScoreMatching: 'a' in: input.
	
	self assertCollection: output asArray equals: #('a' 'ab' 'abc' 'cab' 'cat')
	