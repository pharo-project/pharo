initialization
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel

	self model: anObject.
	getListSelector := getListSel.
	getSelectionSelector := getSelectionSel.
	setSelectionSelector := setSelectionSel.
	getMenuSelector := getMenuSel.
	keystrokeActionSelector := keyActionSel.
	autoDeselect := true.
	self borderWidth: 1.
	self list: self getList.