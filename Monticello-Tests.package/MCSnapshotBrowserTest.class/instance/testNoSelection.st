testing
testNoSelection
	self assertAListMatches: self allCategories.
	self denyAListIncludesAnyOf: self definedClasses.
	self denyAListIncludesAnyOf: self allProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	"and if there I need to see the packages scripts (or none)"
	self assertTextIs: '(package defines no scripts)'.