tests
testUnequalSize
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem / 'plonk' / 'griffle'.
	self deny: a = b.