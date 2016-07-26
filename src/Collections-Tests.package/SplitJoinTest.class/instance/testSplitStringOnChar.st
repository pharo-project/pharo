running
testSplitStringOnChar
	self assert: ('does eat oats and lambs eat oats' splitOn: Character space)
		equals: #('does' 'eat' 'oats' 'and' 'lambs' 'eat' 'oats') asOrderedCollection