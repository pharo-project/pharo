initialization
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel  wrapSelector: wrapSel
	self model: anObject.
	getListSelector := getListSel.
	getIndexSelector := getSelectionSel.
	setIndexSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	wrapSelector := wrapSel.
	self autoDeselect: true.
	self borderWidth: 1.
	self updateList.
	self selectionIndex: self getCurrentSelectionIndex.
	self initForKeystrokes