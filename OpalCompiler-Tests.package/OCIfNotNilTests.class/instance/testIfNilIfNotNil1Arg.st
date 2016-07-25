tests
testIfNilIfNotNil1Arg

	self assert: (5 ifNil: [#foo] ifNotNil: [:a | a printString]) = '5'.
	self assert: (nil ifNil: [#foo] ifNotNil: [:a | a printString]) = #foo