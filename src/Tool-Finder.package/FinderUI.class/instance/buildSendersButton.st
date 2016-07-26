items creation
buildSendersButton
	^ (PluggableButtonMorph 
			on: self 
			getState: #sendersButtonState 
			action: #sendersButtonAction 
			label: #sendersButtonLabel)
			hResizing: #spaceFill