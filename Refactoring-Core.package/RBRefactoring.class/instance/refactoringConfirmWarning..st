private
refactoringConfirmWarning: aString
	| ret |
	ret := UIManager default confirm: aString orCancel: [ ^ RBRefactoringFailure signal: 'aborted' ].
	^ ret