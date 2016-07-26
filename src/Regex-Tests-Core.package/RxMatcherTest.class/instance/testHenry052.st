testing-henry
testHenry052
	self runRegex: #('[[:digit:]e]+'
		'01234' true (1 '01234')
		'0123e456' true (1 '0123e456')
		'0123e45g78' true (1 '0123e45'))