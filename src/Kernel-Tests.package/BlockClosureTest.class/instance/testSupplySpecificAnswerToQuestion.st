tests
testSupplySpecificAnswerToQuestion

	self should: [false = ([self confirm: 'You like Smalltalk?'] 
		valueSupplyingAnswer: #('You like Smalltalk?' false))]