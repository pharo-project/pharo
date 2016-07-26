compatibility tests
testContainsLocator
	locator := FileLocator image.
	self assert: (locator contains: locator / 'griffle').