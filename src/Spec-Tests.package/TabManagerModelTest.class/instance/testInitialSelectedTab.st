tests
testInitialSelectedTab
	"issue 15145"

	self assert: manager selectedTab equals: nil.
	manager addTab: redTab.
	self assert: manager selectedTab equals: redTab.
	manager addTab: self redMorphTab.
	self assert: manager selectedTab equals: redTab