compatibility tests
testCommaAddsExtensionAgain
	locator := FileLocator image / 'griffle.plonk'.
	self assert: (locator , 'nurp') basename = 'griffle.plonk.nurp'