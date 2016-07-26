items creation
buildBrowseButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #browseButtonState 
			action: #browseButtonAction 
			label: #browseButtonLabel)
			hResizing: #spaceFill