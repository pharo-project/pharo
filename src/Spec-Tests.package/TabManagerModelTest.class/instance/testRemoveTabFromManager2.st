tests
testRemoveTabFromManager2
	"issue 11461 -	update tab collection in the model when tab has been removed in morphic"

	manager addTab: redTab.
	self assertCollection: manager tabs hasSameElements: {redTab}.

	window := manager openWithSpec.

	redTab widget widget delete.

	self assertCollection: manager tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: manager) hasSameElements: #()