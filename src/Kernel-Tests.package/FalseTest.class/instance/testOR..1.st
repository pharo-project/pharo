tests - logical operations
testOR

	self assert: (false | true).
	self deny: (false | false).