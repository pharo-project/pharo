tests - fixture
test0FixtureIncludeTest
	| anElementIn |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self elementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self anotherElementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self anotherElementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self empty.
	self assert: self empty isEmpty