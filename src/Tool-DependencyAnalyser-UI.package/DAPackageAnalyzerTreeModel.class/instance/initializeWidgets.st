initialization
initializeWidgets
	super initializeWidgets.
	
	packageLabel := self newLabel label: 'Analysis of packages'.
	
	textPackageField := self newTextInput
		ghostText: 'Enter a package name';
		entryCompletion: self packagesEntryCompletion.
	
	self initializeButtons