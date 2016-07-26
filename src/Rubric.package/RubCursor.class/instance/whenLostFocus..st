event handling
whenLostFocus: anAnnouncement
	self stayVisible
		ifFalse: [ 
			self removeAlarm.
			self beNotVisible ]