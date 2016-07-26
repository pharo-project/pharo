tests
testIncludesAll
	self assert: (aTimespan includesAllOf: (Bag with: jan01)).
	self deny: (aTimespan includesAllOf: (Bag with: jan01 with: jan08))
