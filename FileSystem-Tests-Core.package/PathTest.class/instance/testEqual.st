tests
testEqual
	| a b |
	a := Path * 'plonk'.
	b := Path * 'plonk'.
	self deny: a == b.
	self assert: a equals: b.