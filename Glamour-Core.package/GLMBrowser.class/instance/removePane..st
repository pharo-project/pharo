accessing
removePane: aPane
	aPane unregisterFromAllAnnouncements.
	self panes remove: aPane