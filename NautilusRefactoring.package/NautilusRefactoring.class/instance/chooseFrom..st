display
chooseFrom: anArray
	| answer |
	answer := UIManager default chooseFrom: anArray.
	answer = 0 ifTrue: [ RefactoringAborted signal ].
	^ answer