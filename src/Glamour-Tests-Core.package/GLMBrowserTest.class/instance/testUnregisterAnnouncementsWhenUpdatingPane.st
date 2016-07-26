tests
testUnregisterAnnouncementsWhenUpdatingPane
	| announcer browser |
	announcer := Announcer new.
	browser := GLMTabulator new.
	browser row: #one.
	(browser transmit)
		to: #one;
		andShow: [ :presenter | 
					(presenter list)
						updateOn: GLMTestAnnouncement from: [ announcer ];
						display: [ :start | start to: 10 ] ].
	browser startOn: 1.
	browser panes first presentations first registerAnnouncements.
	browser unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0