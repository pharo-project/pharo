testing-henry
testHenry133
	self runRegex: #('(bc+d$|ef*g.|h?i(j|k))' 
		'effgz' true (1 'effgz' 2 'effgz' 3 nil)
		'ij' true (1 'ij' 2 'ij' 3 'j')
		'effg' false nil
		'bcdd' false nil
		'reffgz' true (1 'effgz' 2 'effgz' 3 nil))