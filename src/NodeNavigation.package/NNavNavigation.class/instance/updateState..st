private
updateState: newSelection

	newSelection = currentSelection ifFalse: [ lastSelection := currentSelection].
	currentSelection := newSelection.
	currentEditor := nil.
	