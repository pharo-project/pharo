testing-henry
testHenry109
	self runRegex: #('ab*' 
		'xabyabbbz' true (1 'ab')
		'xayabbbz' true (1 'a'))