running
testSplitStringOnSubstring
	self assert: ('the banana man can can bananas' splitOn: 'an')
		equals: #('the b' '' 'a m' ' c' ' c' ' b' '' 'as') asOrderedCollection