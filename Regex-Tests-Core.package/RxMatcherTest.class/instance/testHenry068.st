testing-henry
testHenry068
	self runRegex: #('[[:xdigit:]]+'
		'  x10FCD  ' true (1 '10FCD')
		' hgfedcba0123456789ABCDEFGH '
			true (1 'fedcba0123456789ABCDEF'))