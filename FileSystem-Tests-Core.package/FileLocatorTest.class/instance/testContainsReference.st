compatibility tests
testContainsReference
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle')).