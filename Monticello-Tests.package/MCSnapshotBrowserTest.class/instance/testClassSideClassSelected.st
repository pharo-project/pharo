testing
testClassSideClassSelected
	self clickOnButton: 'class'.
	self selectMockClassA.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAClassProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self assertTextIs: self classAclassDefinitionString.