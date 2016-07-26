tests
testCommaAddsExtensionAgain
	| path result |
	path := Path * 'plonk.griffle'.
	result := path, 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'