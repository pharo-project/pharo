tests - testing
testIsDictionary
	self deny: Object new isDictionary.
	self assert: nonEmptyDict isDictionary.
	self assert: emptyDict isDictionary.