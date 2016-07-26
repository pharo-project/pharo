tests
testRemoveTabUnsetOwner
	"issue 16553"

	manager addTab: redTab.
	self assert: redTab owner equals: manager.
	manager removeTab: redTab.
	self assert: redTab owner equals: nil