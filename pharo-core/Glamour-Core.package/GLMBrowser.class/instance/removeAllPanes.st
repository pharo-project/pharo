accessing
removeAllPanes
	self panes do: [:each | each unregisterFromAllAnnouncements].
	^ self panes removeAll