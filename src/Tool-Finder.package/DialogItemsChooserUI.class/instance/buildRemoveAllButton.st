build items
buildRemoveAllButton
	"Answer a new button for removing all items from the
	selected list and adding to the unselected list."
	
	^(self
		newButtonFor: self
		getState: #removeAllButtonState
		action: #removeAllButtonAction
		arguments: nil
		getEnabled: #hasSelectedItems
		getLabel: #removeAllButtonLabel
		help: nil)
		hResizing: #spaceFill