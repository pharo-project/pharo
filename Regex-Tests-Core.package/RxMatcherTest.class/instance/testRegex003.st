testing
testRegex003
	self runRegex: #('a\W+c'
		' abb_bbc ' false nil
		'abb-bc' false nil
		'a.,:;-&!"#%/()={[]}+?\~*''c' true (1 'a.,:;-&!"#%/()={[]}+?\~*''c'))