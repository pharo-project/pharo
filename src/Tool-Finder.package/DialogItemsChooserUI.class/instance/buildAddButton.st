build items
buildAddButton
	"Answer a new button for removing selected items
	from the unselected list and adding to the selected list."
	
	^(self
		newButtonFor: self
		getState: #addButtonState
		action: #addButtonAction 
		arguments: nil
		getEnabled: #hasUnselectedSelections
		getLabel: #addButtonLabel
		help: nil)
		hResizing: #spaceFill