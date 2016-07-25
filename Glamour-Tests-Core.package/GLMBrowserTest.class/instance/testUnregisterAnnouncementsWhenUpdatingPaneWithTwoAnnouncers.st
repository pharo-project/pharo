tests
testUnregisterAnnouncementsWhenUpdatingPaneWithTwoAnnouncers
	| announcer browser announcer2 |
	announcer := Announcer new.
	announcer2 := Announcer new.
	browser := GLMTabulator new.
	browser row: #one.
	(browser transmit)
		to: #one;
		andShow: [ :presenter | 
					(presenter list)
						updateOn: GLMTestAnnouncement
							fromAll: [ 
									{announcer.
									nil.
									announcer2} ];
						display: [ :start | start to: 10 ] ].
	browser startOn: 1.
	browser panes first presentations first registerAnnouncements.
	browser unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0.
	self assert: announcer2 numberOfSubscriptions equals: 0