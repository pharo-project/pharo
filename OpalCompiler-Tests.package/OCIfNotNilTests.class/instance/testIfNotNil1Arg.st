tests
testIfNotNil1Arg
	self assert: (5 ifNotNil: [ :a | a printString ]) = '5'.
	self assert: (nil ifNotNil: [ :a | a printString ]) isNil