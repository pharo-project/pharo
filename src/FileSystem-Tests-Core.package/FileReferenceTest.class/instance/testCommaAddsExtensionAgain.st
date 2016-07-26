tests
testCommaAddsExtensionAgain
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref, 'nurp'.
	self assert: result basename = 'plonk.griffle.nurp'