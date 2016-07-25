testing-henry
testHenry069
	self runRegex: #('ab|cd'
		'abc' true (1 'ab')
		'abcd' true (1 'ab'))