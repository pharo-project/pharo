public
unregisterFromAllAnnouncements
	self announcerObjects do: [:each | 
		[each unsubscribe: self] on: Error do: [:e | e resume]] 