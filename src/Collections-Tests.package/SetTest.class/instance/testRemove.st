tests - sunit original tests
testRemove
	full remove: 4.
	self assert: (full includes: 3).
	self deny: (full includes: 4).