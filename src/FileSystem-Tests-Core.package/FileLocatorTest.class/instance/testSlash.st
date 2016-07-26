compatibility tests
testSlash
	locator := FileLocator image / 'griffle'.
	self assert: locator = (FileLocator image / 'griffle')