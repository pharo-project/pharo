tests - sunit original tests
testIncludes
	self assert: (full includes: self elementInForIncludesTest).
	self deny: (full includes: self elementNotIn).
			