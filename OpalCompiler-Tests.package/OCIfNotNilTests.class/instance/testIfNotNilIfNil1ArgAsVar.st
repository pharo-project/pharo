tests
testIfNotNilIfNil1ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5 ifNotNil: block2 ifNil: block1) = '5'.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo