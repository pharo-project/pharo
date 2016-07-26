tests
testSupplyAnswerUsingOnlySubstringOfQuestion

	self should: [false = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('like' false))]