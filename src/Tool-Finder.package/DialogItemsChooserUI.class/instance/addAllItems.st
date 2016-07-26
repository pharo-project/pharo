buttons behavior
addAllItems

	| selection |
	selection := self unselectedItemsProbablyRestricted.
	selection ifEmpty: [^self].
	self unselectedItems removeAll: selection.
	self selectedItems addAll: selection.
	unselectedSelectionList removeAll.
	self unselectedSelectionIndex: 0.
	self
		changed: #selectedItemsProbablyRestricted;
		changed: #hasSelectedItems;
		changed: #unselectedItemsProbablyRestricted;
		changed: #hasUnselectedItems;
		changed: #hasUnselectedSelections