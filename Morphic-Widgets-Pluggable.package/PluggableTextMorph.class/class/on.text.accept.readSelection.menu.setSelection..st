instance creation
on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel setSelection: setSelectionSel

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: setSelectionSel