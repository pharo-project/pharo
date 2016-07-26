resolution tests
testResolveRelativeReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	self flag: 'this is a bit weird...'.
	reference := FileSystem memory * 'griffle'.
	result := locator resolve: reference..
	self assert: result class= locator class.
	self assert: result origin = locator origin.
	self assert: result path = reference path.