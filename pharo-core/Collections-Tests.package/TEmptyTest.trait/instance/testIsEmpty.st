tests - empty
testIsEmpty

	self assert: (self empty isEmpty).
	self deny: (self nonEmpty isEmpty).