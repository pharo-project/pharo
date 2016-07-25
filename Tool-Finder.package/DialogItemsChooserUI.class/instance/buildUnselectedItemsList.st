build items
buildUnselectedItemsList

	^(PluggableListMorph
		on: self 
		list: #unselectedItemsProbablyRestricted 
		primarySelection: #unselectedSelectionIndex
		changePrimarySelection: #unselectedSelectionIndex:
		listSelection: #unselectedSelectionAt:
		changeListSelection: #unselectedSelectionAt:put:
		menu: nil)
			hResizing: #spaceFill;
			vResizing: #spaceFill.