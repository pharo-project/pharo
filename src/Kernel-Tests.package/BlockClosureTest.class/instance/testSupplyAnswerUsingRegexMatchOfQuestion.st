tests
testSupplyAnswerUsingRegexMatchOfQuestion

	(String includesSelector: #matchesRegex:) ifFalse: [^ self].
	
	self should: [true = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('.*Smalltalk\?' true))]