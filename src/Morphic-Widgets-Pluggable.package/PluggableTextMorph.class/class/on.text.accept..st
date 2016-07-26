instance creation
on: anObject text: getTextSel accept: setTextSel

	^ self on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: nil
		menu: nil