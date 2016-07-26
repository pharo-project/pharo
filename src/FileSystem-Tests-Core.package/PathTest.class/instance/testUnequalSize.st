tests
testUnequalSize
	| a b |
	a := Path * 'plonk'.
	b := (Path * 'plonk') / 'griffle'.
	self deny: a = b.