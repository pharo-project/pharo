compatibility tests
testWithExtensionAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'