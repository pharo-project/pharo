announcements
announceOpened
	self doAnnounce: (MorphOpened morph: self).
	self submorphs do: #announceOpened