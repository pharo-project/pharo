tests
testUpdateOnPortsUpdate
	| col pane presentation announcer |
	col := OrderedCollection with: 1 with: 2.
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	presentation entity: col.
	presentation shouldValidate: true.
	presentation registerAnnouncements.
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	col removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: presentation selection isNil