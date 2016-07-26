testing
testMethodSelected
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	self clickOnListItem: 'falsehood'.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self assertAListMatches: self classABooleanMethods.
	self assertTextIs: self falsehoodMethodSource.