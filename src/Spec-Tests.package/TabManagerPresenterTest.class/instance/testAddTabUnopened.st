tests
testAddTabUnopened
	self assertCollection: testedInstance tabs hasSameElements: #().
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}