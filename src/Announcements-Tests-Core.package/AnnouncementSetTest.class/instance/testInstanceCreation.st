tests
testInstanceCreation
	| set |
	set := AnnouncementMockA , AnnouncementMockB.
	self assert: set size equals: 2