tests
testUnequalContent
	| a b |
	a := Path * 'plonk'.
	b := Path * 'griffle'.
	self deny: a = b.