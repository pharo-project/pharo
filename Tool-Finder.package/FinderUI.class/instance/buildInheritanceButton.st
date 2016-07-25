items creation
buildInheritanceButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #inheritanceButtonState 
			action: #inheritanceButtonAction 
			label: #inheritanceButtonLabel)
			hResizing: #spaceFill