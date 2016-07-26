menu
setLabel

	| newLabel |
	newLabel := UIManager default
		request: 'Please enter a new label for this button' translated
		initialAnswer: self label.
	newLabel isEmptyOrNil ifFalse: [self labelString: newLabel].
