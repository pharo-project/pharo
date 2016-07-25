testing
testRemove
	full remove: 5.
	self assert: (full includes: #abc).
	self deny: (full includes: 5)