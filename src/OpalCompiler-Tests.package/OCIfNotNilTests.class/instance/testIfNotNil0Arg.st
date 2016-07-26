tests
testIfNotNil0Arg
	self assert: (5 ifNotNil: [ #foo ]) = #foo.
	self assert: (nil ifNotNil: [ #foo ]) isNil