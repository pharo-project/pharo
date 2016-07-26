building internal
setMiddlePaneButtonsTo: aValidator

	aValidator validator: 'Cancel' act: [:input |
		self cancelButtonAction 
		].
	aValidator validator: self backSign act: [:input |
		self previousButtonAction
		].
	aValidator validator: self forwardSign act: [:input |
		self nextButtonAction
		].
	