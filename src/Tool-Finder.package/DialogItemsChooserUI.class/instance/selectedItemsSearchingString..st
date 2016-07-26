accessing
selectedItemsSearchingString: anObject

	selectedItemsSearchingString := anObject.
	selectedSelectionList removeAll.
	self selectedSelectionIndex: 0.
	self
		changed: #selectedItemsProbablyRestricted;
		changed: #selectedSelectionAt:;
		changed: #selectedSelectionIndex;
		changed: #hasSelectedSelections