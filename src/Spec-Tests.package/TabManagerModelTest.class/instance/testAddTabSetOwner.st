tests
testAddTabSetOwner
	"issue #16553"

	self assert: redTab owner equals: nil.
	testedInstance addTab: redTab.
	self assert: redTab owner equals: testedInstance