tests
testIfNotNil0ArgAsVar
	| block |
	block := [ #foo ].
	self assert: (5 ifNotNil: block) = #foo.
	self assert: (nil ifNotNil: block) isNil