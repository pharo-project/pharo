testing-henry
testHenry034
	self runRegex: #('a[bc]d'
		'abc' false nil
		'abd' true (1 'abd'))