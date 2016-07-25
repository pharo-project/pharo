display
addAll: aWindow

	aWindow
		addMorph: self buildInformationList
		fullFrame: self categoriesFrame.
		
	aWindow
		addMorph: self buildInformationText
		fullFrame: self reportFrame.