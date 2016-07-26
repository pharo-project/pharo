resolution tests
testResolveCompoundString
	| result compound |
	locator := FileLocator image / 'plonk'.
	compound := 'griffle', locator fileSystem delimiter asString, 'nurp'.
	result := locator resolve: compound.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = ((Path * 'plonk') / 'griffle' / 'nurp')