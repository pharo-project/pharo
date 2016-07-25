initialization
initializeWidgets
	self
		initializeLists;
		initializeLabels;
		initializeButtons;
		buildList.
		
	textPackageField := self newTextInput
		ghostText: 'Filter package by name';
		entryCompletion: self packagesEntryCompletion.