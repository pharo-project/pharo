tests
testChangeLabel
	"issue 14885"

	manager addTab: redTab.

	self assert: redTab label = 'Red Morph'.
	redTab label: 'Change Before Opening'.
	self assert: redTab label = 'Change Before Opening'.

	window := manager openWithSpec.
	redTab label: 'Change After Opening'.
	self assert: redTab label = 'Change After Opening'