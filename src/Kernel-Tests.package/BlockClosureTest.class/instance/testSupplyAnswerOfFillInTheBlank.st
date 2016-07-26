testing
testSupplyAnswerOfFillInTheBlank

	self should: ['blue' = ([UIManager default request: 'Your favorite color?'] 
		valueSupplyingAnswer: #('Your favorite color?' 'blue'))]