testing-henry
testHenry135
	self runRegex: #('multiple words of text' 
		'uh-uh' false nil
		'multiple words of text, yeah' true (1 'multiple words of text'))