instance creation
on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel
	^ self new
		on: anObject
		list: listSel
		primarySelection: getSelectionSel
		changePrimarySelection: setSelectionSel
		listSelection: getListSel
		changeListSelection: setListSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default"