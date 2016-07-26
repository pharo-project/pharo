tests
testAddRaisesAnnouncement
	| composite announcementCount pane |
	composite := GLMCompositePresentation new.
	pane := composite pane.
	announcementCount := 0.
	composite entity: 42.
	pane when: GLMMatchingPresentationsChanged do: [:ann | 
		announcementCount := announcementCount + 1 ].
	composite add: GLMPresentation new.
	self assert: announcementCount equals: 1