items creation
buildRemoveButton

	^  (PluggableButtonMorph
			on: self 
			getState: #removeButtonState
			action: #removeButtonAction
			label: #removeButtonLabel) hResizing: #spaceFill; vResizing: #shrinkWrap