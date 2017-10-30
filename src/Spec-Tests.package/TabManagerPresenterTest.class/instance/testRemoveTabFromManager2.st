tests
testRemoveTabFromManager2
	"issue 11461 -	update tab collection in the model when tab has been removed in morphic"

	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	self openInstance .
	redTab widget widget delete.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()