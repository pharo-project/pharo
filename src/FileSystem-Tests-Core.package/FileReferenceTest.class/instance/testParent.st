tests
testParent
	| ref parent |
	ref := filesystem * 'plonk' / 'griffle'.
	parent := ref parent.
	self assert: parent class = ref class.
	self assert: (parent path at: 1) = 'plonk'