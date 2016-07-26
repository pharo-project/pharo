testing-henry
testHenry062
	self runRegex: #('[[:upper:]]+'
		'UPPERlower1234' true (1 'UPPER')
		'lowerUPPER ' true (1 'UPPER'))