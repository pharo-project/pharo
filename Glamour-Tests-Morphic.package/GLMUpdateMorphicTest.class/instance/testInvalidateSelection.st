tests
testInvalidateSelection
	(browser panes first port: #selection) value: 3.
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: (browser panes first port: #selection) value isNil.
	self assert: listMorph model selection isNil