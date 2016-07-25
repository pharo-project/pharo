tests
testAddTabSetOwner
	"issue #16553"

	self assert: redTab owner equals: nil.
	manager addTab: redTab.
	self assert: redTab owner equals: manager