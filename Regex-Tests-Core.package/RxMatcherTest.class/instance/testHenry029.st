testing-henry
testHenry029
	self runRegex: #('\Ba\w+' "a word with an A in the middle, match at A and further"
		'land ancient' true (1 'and')
		'antique vase' true (1 'ase')
		'smalltalk shall overcome' true (1 'alltalk')
		'foonix is better' false nil)