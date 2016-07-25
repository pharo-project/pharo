initialization
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel wrapSelector: wrapSel
	self model: anObject.
	self getListSelector: getListSel.
	self getIndexSelector: getSelectionSel.
	self setIndexSelector: setSelectionSel.
	self getMenuSelector: getMenuSel.
	self keystrokeActionSelector: keyActionSel.
	self basicWrapSelector: wrapSel
