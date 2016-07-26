private-events
loadStopped: anAnnouncement
	self withoutExceptionsDo: [ 
		self startSystemAnnouncements ]