tests
testAddTabOpened
	window := manager openWithSpec.

	self assertCollection: manager tabs hasSameElements: #().
	manager addTab: redTab.
	self assertCollection: manager tabs hasSameElements: {redTab}