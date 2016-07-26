tests
testAddTabUnopened
	self assertCollection: manager tabs hasSameElements: #().
	manager addTab: redTab.
	self assertCollection: manager tabs hasSameElements: {redTab}