testing
testRegex004
	self runRegex: #(':isVowel:'
		'aei' true nil
		'xyz' false nil)