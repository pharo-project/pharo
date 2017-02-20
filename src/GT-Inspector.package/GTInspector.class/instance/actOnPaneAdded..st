callbacks
actOnPaneAdded: anAnnouncement
	
	"Update toolbar of a pane before the last (added) pane if there is one"
	(self panes atLast: 2 ifAbsent: [ ^ self ])
		updateToolbar