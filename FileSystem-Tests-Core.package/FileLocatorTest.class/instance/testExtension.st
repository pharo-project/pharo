compatibility tests
testExtension
	locator := FileLocator image, 'bak'.
	self assert: (locator basename endsWith: '.bak')	