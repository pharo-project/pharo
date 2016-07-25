testing-henry
testHenry050
	self runRegex: #('[e\d]+' "reversal of the above, should be the same"
		'01234' true (1 '01234')
		'0123e456' true (1 '0123e456')
		'0123e45g78' true (1 '0123e45'))