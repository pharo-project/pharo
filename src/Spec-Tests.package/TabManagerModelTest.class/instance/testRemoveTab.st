tests
testRemoveTab
	"issue 11461"

	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	testedInstance removeTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: #()