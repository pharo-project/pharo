tests
testAsAbsoluteIdentity
	| ref |
	ref := filesystem / 'plonk'.
	self assert: ref asAbsolute == ref