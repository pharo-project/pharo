display
request: requestString initialAnswer: initialAnswerString
	| answer |
	answer := UIManager default request: requestString initialAnswer: initialAnswerString.
	answer isEmptyOrNil ifTrue:  [ RefactoringAborted signal ].
	^ answer
	
