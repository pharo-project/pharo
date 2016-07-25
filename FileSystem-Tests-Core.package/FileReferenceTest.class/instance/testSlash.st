tests
testSlash
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref / 'griffle'.
	self assert: result class = ref class.
	self assert: result  isRelative.
	self assert: (result path at: 1) = 'plonk'.
	self assert: (result path at: 2) = 'griffle'.
