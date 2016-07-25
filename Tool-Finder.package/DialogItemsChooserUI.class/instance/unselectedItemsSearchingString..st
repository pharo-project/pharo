accessing
unselectedItemsSearchingString: anObject

	unselectedItemsSearchingString := anObject.
	unselectedSelectionList removeAll.
	self unselectedSelectionIndex: 0.
	self
		changed: #unselectedItemsProbablyRestricted;
		changed: #unselectedSelectionAt:;
		changed: #unselectedSelectionIndex;
		changed: #hasUnselectedSelections