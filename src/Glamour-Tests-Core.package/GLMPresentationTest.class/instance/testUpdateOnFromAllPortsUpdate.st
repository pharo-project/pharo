tests
testUpdateOnFromAllPortsUpdate
	| pane presentation announcers col updated |
	col := (1 to: 4) asOrderedCollection. 
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	announcers := col collect: [:each | Announcer new].
	pane addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement fromAll: [ announcers ].
	presentation when: GLMPresentationUpdated do: [:a | updated := true ].
	presentation entity: col.
	presentation shouldValidate: true.
	presentation registerAnnouncements.

	updated := false.
	announcers last announce: GLMTestAnnouncement.
	self assert: updated.
	updated := false.
	announcers last announce: GLMTestAnnouncement.
	self assert: updated.
