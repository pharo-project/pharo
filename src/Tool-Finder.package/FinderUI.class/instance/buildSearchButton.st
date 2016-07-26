items creation
buildSearchButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #searchButtonState 
			action: #searchButtonAction 
			label: #searchButtonLabel)
			hResizing: #shrinkWrap