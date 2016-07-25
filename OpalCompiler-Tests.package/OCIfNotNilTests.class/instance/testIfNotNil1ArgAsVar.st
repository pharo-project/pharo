tests
testIfNotNil1ArgAsVar
	| block |
	block := [ :a | a printString ].
	self assert: (5 ifNotNil: block) = '5'.
	self assert: (nil ifNotNil: block) isNil