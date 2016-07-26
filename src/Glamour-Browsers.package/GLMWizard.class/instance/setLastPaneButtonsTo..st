building internal
setLastPaneButtonsTo: aValidator

	aValidator validator:  'Cancel' act:  [:input |
		self cancelButtonAction
		].
	aValidator validator: self backSign act: [:input |
		self previousButtonAction
		].
	aValidator validator: 'Finish' act:  [:input |
		self terminateButtonAction
		].