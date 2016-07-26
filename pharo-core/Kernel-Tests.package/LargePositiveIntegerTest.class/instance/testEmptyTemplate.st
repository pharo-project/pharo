tests
testEmptyTemplate

	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '0'.
	self assert: i normalize = 0