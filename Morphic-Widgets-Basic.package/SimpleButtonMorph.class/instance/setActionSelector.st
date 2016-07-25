menu
setActionSelector

	| newSel |
	newSel := UIManager default
		request:
'Please type the selector to be sent to
the target when this button is pressed' translated
		initialAnswer: actionSelector.
	newSel isEmptyOrNil ifFalse: [self actionSelector: newSel].
