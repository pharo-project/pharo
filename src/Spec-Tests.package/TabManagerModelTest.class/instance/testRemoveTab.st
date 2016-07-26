tests
testRemoveTab
	"issue 11461"

	manager addTab: redTab.
	self assertCollection: manager tabs hasSameElements: {redTab}.

	manager removeTab: redTab.
	self assertCollection: manager tabs hasSameElements: #()