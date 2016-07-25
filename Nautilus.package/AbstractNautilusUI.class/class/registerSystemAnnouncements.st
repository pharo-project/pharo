class initialization
registerSystemAnnouncements
	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		when: IconSetChanged
		send: #resetIconCaches
		to: self