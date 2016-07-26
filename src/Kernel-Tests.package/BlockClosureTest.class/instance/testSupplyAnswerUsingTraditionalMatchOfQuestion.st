tests
testSupplyAnswerUsingTraditionalMatchOfQuestion

	self should: [true = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('*Smalltalk#' true))]