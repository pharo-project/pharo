tests - empty
testIsEmptyOrNil

	self assert: (self empty isEmptyOrNil).
	self deny: (self nonEmpty isEmptyOrNil).