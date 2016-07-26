build items
buildAddAllButton
	"Answer a new button for removing all items
	form the unslected list and adding to the selected list."
	
	^(self
		newButtonFor: self
		getState: #addAllButtonState
		action: #addAllButtonAction 
		arguments: nil
		getEnabled: #hasUnselectedItems
		getLabel: #addAllButtonLabel
		help: nil)
		hResizing: #spaceFill