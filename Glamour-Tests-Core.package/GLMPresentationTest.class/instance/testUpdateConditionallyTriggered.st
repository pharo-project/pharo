tests
testUpdateConditionallyTriggered
	| pane presentation announcer timesTriggered |
	pane := GLMPane named: 'test'.
	(pane port: #entity) value: 40.
	presentation := GLMPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation when: GLMPresentationUpdated do: [ :announcement | timesTriggered := timesTriggered + 1 ].
	presentation updateOn: GLMTestAnnouncement from: [ :x | announcer ] when: [ :ann :entity | ann parameter + entity = 42 ].
	presentation registerAnnouncements.
	announcer announce: (GLMTestAnnouncement new parameter: 0).
	self assert: timesTriggered equals: 0.
	announcer announce: (GLMTestAnnouncement new parameter: 2).
	self assert: timesTriggered equals: 1