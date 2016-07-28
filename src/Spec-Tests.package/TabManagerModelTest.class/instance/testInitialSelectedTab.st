tests
testInitialSelectedTab
	"issue 15145"

	self assert: testedInstance selectedTab equals: nil.
	testedInstance addTab: redTab.
	self assert: testedInstance selectedTab equals: redTab.
	testedInstance addTab: self redMorphTab.
	self assert: testedInstance selectedTab equals: redTab