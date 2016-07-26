compatibility tests
testWithExtensionReplacesExtension
	locator := FileLocator image / 'griffle.nurp'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'