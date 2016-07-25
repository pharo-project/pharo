compatibility tests
testEqual
	| a b |
	a := FileLocator image.
	b := FileLocator image.
	self deny: a == b.
	self assert: a = b.