building internal
setFirstPaneButtonsTo: aValidator

	aValidator validator: 'Cancel' act:  [:input |
		self cancelButtonAction
		] .
	aValidator validator: self forwardSign act: [:input |
		self nextButtonAction 
		] populatePorts: #(#selection).