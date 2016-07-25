helpers
subscribeOn: anAnnouncement

	SystemAnnouncer uniqueInstance weak 
		when: anAnnouncement
		send: #add: 
		to: collectedAnnouncements