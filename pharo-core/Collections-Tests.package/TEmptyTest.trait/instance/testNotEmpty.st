tests - empty
testNotEmpty

	self assert: (self nonEmpty  notEmpty).
	self deny: (self empty notEmpty).