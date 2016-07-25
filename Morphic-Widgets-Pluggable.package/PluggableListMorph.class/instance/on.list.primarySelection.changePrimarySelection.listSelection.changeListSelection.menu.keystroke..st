multi-selection
on: anObject list: listSel primarySelection: getSelectionSel changePrimarySelection: setSelectionSel listSelection: getListSel changeListSelection: setListSel menu: getMenuSel keystroke: keyActionSel
	"setup a whole load of pluggability options"
	getSelectionListSelector := getListSel.
	setSelectionListSelector := setListSel.
	self on: anObject list: listSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel.
	self beMultipleSelection.
	^ self
