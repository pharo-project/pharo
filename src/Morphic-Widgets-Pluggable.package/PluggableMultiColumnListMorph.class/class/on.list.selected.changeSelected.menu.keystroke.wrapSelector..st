instance creation
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel wrapSelector: wrapSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors."

	^ self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel
		wrapSelector: wrapSel