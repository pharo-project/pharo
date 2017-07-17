tests
testIncludesAll
	self assert: (aTimespan includesAll: (Bag with: jan01)).
	self deny: (aTimespan includesAll: (Bag with: jan01 with: jan08))
