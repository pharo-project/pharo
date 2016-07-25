items creation
buildEnvironmentButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #environmentButtonState 
			action: #environmentButtonAction 
			label: #environmentButtonLabel)
			beSticky