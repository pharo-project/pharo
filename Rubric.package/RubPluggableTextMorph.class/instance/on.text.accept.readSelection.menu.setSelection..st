model protocol
on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel
	self
		on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel.
	setSelectionSelector := setSelectionSel.
	self borderWidth: 1.
	self setText: self getTextFromModel.
	self setSelection: self getSelectionFromModel