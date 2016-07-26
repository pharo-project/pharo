private-events
loadStarted: anAnnouncement
	self withoutExceptionsDo: [
		self stopSystemAnnouncements.
		self resetQuickly " we must get rid of all example objects lying around - this is really important for consistency " ]