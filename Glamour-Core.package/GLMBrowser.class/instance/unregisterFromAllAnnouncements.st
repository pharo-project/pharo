updating
unregisterFromAllAnnouncements
	super unregisterFromAllAnnouncements.
	self panes do: [:each | 
		each unregisterFromAllAnnouncements ]