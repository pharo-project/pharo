callbacks
actOnPaneRemoved: anAnnouncement

	self panes last presentations first updateToolbar.
	self notifyPaneRemoved ifTrue: [ 
		self debugger announce: anAnnouncement ]