tests
testIfNotNilIfNil1Arg

	self assert: (5 ifNotNil: [:a | a printString] ifNil: [#foo]) = '5'.
	self assert: (nil ifNotNil: [:a | a printString] ifNil: [#foo]) = #foo