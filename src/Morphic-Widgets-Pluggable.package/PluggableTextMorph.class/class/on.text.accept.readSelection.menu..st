instance creation
on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel

	^ self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel
		setSelection: nil