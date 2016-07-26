accessing
removeLastPane
	self panes last unregisterFromAllAnnouncements.
	^ self panes removeLast