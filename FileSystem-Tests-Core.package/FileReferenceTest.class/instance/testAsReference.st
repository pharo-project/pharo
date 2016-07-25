tests
testAsReference
	| ref |
	ref := filesystem * 'plonk'.
	self assert: ref asFileReference == ref