resolution tests
testResolvePath
	| result path |
	locator := FileLocator image / 'plonk'.
	result := locator resolve: (Path * 'griffle').
	path := (Path * 'plonk') / 'griffle'.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = path.