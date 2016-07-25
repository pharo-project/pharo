tests
testHash

	self assert: atPragma hash = atPragma hash.

	self assert: atPragma hash = anotherAtPragma hash.
	self assert: anotherAtPragma hash = atPragma hash.

	self assert: atPragma hash = anotherAtPragma hash.
	self assert: anotherAtPragma hash = yetAnotherAtPragma hash.
	self assert: yetAnotherAtPragma hash = atPragma hash.