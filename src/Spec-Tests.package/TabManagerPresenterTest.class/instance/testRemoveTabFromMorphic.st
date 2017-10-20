tests
testRemoveTabFromMorphic
	"issue 11461 - remove tab from morphic when it was removed from model"

	testedInstance addTab: redTab.
	self openInstance.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	testedInstance removeTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()