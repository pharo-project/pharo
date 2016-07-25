tests
testUnequalContent
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'griffle'.
	self deny: a = b.