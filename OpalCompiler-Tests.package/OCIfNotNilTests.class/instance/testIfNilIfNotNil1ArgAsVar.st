tests
testIfNilIfNotNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5 ifNil: block1 ifNotNil: block2) = '5'.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo