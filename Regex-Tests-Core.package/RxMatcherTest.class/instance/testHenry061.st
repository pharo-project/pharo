testing-henry
testHenry061
	self runRegex: #('[[:lower:]]+'
		'UPPERlower1234' true (1 'lower')
		'lowerUPPER' true (1 'lower'))