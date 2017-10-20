tests - sunit original tests
testIncludes
	self assert: (full includes: 4).
	self assert: (full includes: 3).
	self deny: (full includes: 6).
			