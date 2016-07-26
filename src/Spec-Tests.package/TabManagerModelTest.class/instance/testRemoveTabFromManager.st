tests
testRemoveTabFromManager
	"issue 11461 - update tab collection in the model when tab has been removed in morphic"

	window := manager openWithSpec.

	manager addTab: redTab.
	self assertCollection: manager tabs hasSameElements: {redTab}.
	self
		assertCollection: (self morphicTabsFor: manager)
		hasSameElements: {redTab widget widget}.

	redTab widget widget delete.

	self assertCollection: manager tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: manager) hasSameElements: #()