building internal
setSinglePaneButtonsTo: aValidator

	aValidator validator: 'Cancel' act: [:input |
		self cancelButtonAction
		].
	aValidator validator: 'Finish' act: [:input |
		self terminateButtonAction
		].
	