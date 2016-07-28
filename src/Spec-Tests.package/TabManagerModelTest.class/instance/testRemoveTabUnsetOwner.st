tests
testRemoveTabUnsetOwner
	"issue 16553"

	testedInstance addTab: redTab.
	self assert: redTab owner equals: testedInstance.
	testedInstance removeTab: redTab.
	self assert: redTab owner equals: nil