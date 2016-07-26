testing-henry
testHenry046
	self runRegex: #('a[^bc]d'
		'aed' true (1 'aed')
		'abd' false nil)