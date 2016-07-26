compatibility tests
testAsAbsolute
	locator := FileLocator image.
	self assert: locator asAbsolute = locator