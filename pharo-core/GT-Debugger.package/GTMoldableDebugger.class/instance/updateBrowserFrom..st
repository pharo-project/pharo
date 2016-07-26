updating
updateBrowserFrom: anAnnouncement
	
	anAnnouncement action needsUpdate ifTrue: [
		self updateBrowser ].
	
	self announcer announce: ( GTDebuggerBrowserUpdateRequest from: anAnnouncement )