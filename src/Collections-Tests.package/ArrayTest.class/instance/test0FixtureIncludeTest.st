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
	self collection.
	self empty.
	self assert: self empty isEmpty.
	self collectionOfFloat.
	self collectionOfFloat do: [ :each | self assert: each isFloat ].
	self elementInForIncludesTest.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementInForIncludesTest ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = true