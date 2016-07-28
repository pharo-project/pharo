tests
testAddTabOpened
	self openInstance.
	self assertCollection: testedInstance tabs hasSameElements: #().
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}