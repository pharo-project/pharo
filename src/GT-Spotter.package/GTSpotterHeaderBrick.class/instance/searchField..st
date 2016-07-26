accessing
searchField: aBrick

	searchField := aBrick.
	
	self themer spotterThemer searchFieldStyleFor: searchField.
	
	searchField morph takeKeyboardFocus.
	
	self addBrickBack: searchField