tests
testChangeLabel
	"issue 14885"

	self redMorphTab.
	manager addTab: testedInstance.
	self assert: testedInstance label = 'Red Morph'.
	testedInstance label: 'Change Before Opening'.
	self assert: testedInstance label = 'Change Before Opening'.
	window := manager openWithSpec.
	testedInstance label: 'Change After Opening'.
	self assert: testedInstance label = 'Change After Opening'