resolution tests
testResolveAbsoluteReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	reference := FileSystem memory / 'griffle'.
	result := locator resolve: reference..
	self assert: result == reference