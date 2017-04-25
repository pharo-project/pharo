callbacks
actOnPaneRemoved: anAnnouncement

	"Update toolbar of a pane before the removed pane (now it is last one)"
	self panes last updateToolbar