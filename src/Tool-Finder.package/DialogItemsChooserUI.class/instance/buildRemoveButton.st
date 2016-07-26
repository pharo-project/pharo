build items
buildRemoveButton
	"Answer a new button for removing selected items
	from the selected list and adding to the unselected list."
	
	^(self
		newButtonFor: self
		getState: #removeButtonState
		action: #removeButtonAction 
		arguments: nil
		getEnabled: #hasSelectedSelections
		getLabel: #removeButtonLabel
		help: nil)
		hResizing: #spaceFill