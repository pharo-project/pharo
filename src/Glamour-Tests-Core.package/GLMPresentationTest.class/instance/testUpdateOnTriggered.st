tests
testUpdateOnTriggered
	| pane presentation announcer timesTriggered |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation when: GLMPresentationUpdated do: [ :announcement | timesTriggered := timesTriggered + 1 ].
	presentation updateOn: GLMTestAnnouncement from: #yourself.
	presentation entity: announcer.
	presentation registerAnnouncements.
	announcer announce: GLMTestAnnouncement.
	self assert: timesTriggered equals: 1