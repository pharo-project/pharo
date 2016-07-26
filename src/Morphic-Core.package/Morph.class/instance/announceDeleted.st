announcements
announceDeleted
	self doAnnounce: (MorphDeleted morph: self).
	self submorphs do: #announceDeleted