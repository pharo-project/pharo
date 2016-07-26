tests
testIfNotNilIfNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5 ifNotNil: block2 ifNil: block1) = #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo