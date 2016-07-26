tests
testEqual

	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'plonk'.
	self deny: a == b.
	self assert: a = b.