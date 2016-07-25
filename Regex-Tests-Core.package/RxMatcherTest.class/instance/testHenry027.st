testing-henry
testHenry027
	self runRegex: #('\ba\w+' "a word beginning with an A"
		'land ancient' true (1 'ancient')
		'antique vase' true (1 'antique')
		'goofy foobar' false nil)