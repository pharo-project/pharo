compatibility tests
testCommaAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator , 'plonk') basename = 'griffle.plonk'