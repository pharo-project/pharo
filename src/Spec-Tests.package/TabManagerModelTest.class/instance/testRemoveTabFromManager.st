tests
testRemoveTabFromManager
	"issue 11461 - update tab collection in the model when tab has been removed in morphic"

	self openInstance.
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: {redTab widget widget}.
	redTab widget widget delete.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()