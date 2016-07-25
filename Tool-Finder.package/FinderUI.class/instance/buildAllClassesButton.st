items creation
buildAllClassesButton
	^ PluggableButtonMorph 
			on: self 
			getState: #allClassesButtonState 
			action: #allClassesButtonAction 
			label: #allClassesButtonLabel