tests
testIfNotNilIfNil0Arg

	self assert: (5 ifNotNil: [#foo] ifNil: [#bar]) = #foo.
	self assert: (nil ifNotNil: [#foo] ifNil: [#bar]) = #bar