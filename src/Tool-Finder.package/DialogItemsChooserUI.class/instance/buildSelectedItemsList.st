build items
buildSelectedItemsList

	^(PluggableListMorph
		on: self 
		list: #selectedItemsProbablyRestricted 
		primarySelection: #selectedSelectionIndex
		changePrimarySelection: #selectedSelectionIndex:
		listSelection: #selectedSelectionAt:
		changeListSelection: #selectedSelectionAt:put:
		menu: nil)
			hResizing: #spaceFill;
			vResizing: #spaceFill.