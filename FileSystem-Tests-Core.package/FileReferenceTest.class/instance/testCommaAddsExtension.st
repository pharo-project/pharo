tests
testCommaAddsExtension
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref, 'griffle'.
	self assert: result basename = 'plonk.griffle'