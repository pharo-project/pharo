selecting
selectionChanged: aSelectionChangedAnnounce
	| selectedWorkingCopies |
	selectedWorkingCopies := aSelectionChangedAnnounce selection selectedItems.
	self info includedPackages: selectedWorkingCopies.
	self changed: #okEnabled