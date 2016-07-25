tests
testUnregisterAnnouncementsWhenRemovingPane
	| announcer presentation browser pane |
	announcer := Announcer new.
	browser := GLMBrowser new.
	pane := GLMPane new.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	browser addPane: pane.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	presentation registerAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 1.
	browser removePane: pane.
	self assert: announcer numberOfSubscriptions equals: 0