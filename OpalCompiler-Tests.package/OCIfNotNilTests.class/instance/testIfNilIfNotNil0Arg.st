tests
testIfNilIfNotNil0Arg

	self assert: (5 ifNil: [#foo] ifNotNil: [#bar]) = #bar.
	self assert: (nil ifNil: [#foo] ifNotNil: [#bar]) = #foo