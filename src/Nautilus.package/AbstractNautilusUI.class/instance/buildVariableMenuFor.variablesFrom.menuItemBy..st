button-variables
buildVariableMenuFor: aMenu variablesFrom: aBlockOrSymbol menuItemBy: aMenuItemBlock

	self selectedClass withAllSuperclasses reverseDo: [ :class |
		| vars |
		
		vars := aBlockOrSymbol value: class. 
		
		self variablesMenuEntry: class 
			items: vars 
			in: aMenu 
			using: aMenuItemBlock.
	].

	