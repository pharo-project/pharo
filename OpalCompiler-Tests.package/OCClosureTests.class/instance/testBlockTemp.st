testing
testBlockTemp
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2