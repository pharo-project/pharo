initialization
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel 
	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	self autoDeselect: true.
	self borderWidth: 1.
	self updateList.
	self selectionIndex: self getCurrentSelectionIndex.
	self initForKeystrokes