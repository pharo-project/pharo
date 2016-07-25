items creation
buildVersionsButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #versionsButtonState 
			action: #versionsButtonAction 
			label: #versionsButtonLabel)
			hResizing: #spaceFill