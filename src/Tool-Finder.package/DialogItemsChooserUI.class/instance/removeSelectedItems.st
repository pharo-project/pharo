buttons behavior
removeSelectedItems

	| selection |
	selection := self selectedSelectionItems.
	selection ifEmpty: [^self].
	self selectedItems removeAll: selection.
	self unselectedItems addAll: selection.
	selectedSelectionList removeAll.
	self selectedSelectionIndex: 0.
	self
		changed: #selectedItemsProbablyRestricted;
		changed: #hasSelectedItems;
		changed: #hasSelectedSelections;
		changed: #unselectedItemsProbablyRestricted;
		changed: #hasUnselectedItems