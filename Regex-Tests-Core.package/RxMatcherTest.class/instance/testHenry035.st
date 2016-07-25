testing-henry
testHenry035
	self runRegex: #('a[b-d]e'
		'abd' false nil
		'ace' true (1 'ace'))