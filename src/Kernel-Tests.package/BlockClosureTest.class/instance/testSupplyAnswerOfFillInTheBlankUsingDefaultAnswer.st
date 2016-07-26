testing
testSupplyAnswerOfFillInTheBlankUsingDefaultAnswer

	self should: ['red' = ([UIManager default  request: 'Your favorite color?' initialAnswer: 'red'] 
		valueSupplyingAnswer: #('Your favorite color?' #default))]