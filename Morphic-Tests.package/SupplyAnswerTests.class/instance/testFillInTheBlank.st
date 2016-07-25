tests
testFillInTheBlank

	| answer |
	answer := [ MorphicUIManager new request: 'Your favorite color?' ] valueSupplyingAnswer: #('Your favorite color?' 'blue').
	
	self assert: answer = 'blue'.