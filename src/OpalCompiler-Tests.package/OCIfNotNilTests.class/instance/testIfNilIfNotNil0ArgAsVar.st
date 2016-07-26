tests
testIfNilIfNotNil0ArgAsVar

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5 ifNil: block1 ifNotNil: block2) = #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo