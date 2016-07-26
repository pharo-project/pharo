tests
testRemoveTabFromMorphic
	"issue 11461 - remove tab from morphic when it was removed from model"

	manager addTab: redTab.
	window := manager openWithSpec.
	self assertCollection: manager tabs hasSameElements: {redTab}.

	manager removeTab: redTab.

	self assertCollection: manager tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: manager) hasSameElements: #()