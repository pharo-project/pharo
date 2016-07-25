tests
testComparision

	self deny: self germanLocaleID = self frenchLocaleID.
	self assert: self germanLocaleID = self germanLocaleID.	