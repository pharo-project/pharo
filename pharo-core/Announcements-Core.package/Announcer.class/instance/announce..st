announce
announce: anAnnouncement

	| announcement |
	announcement := anAnnouncement asAnnouncement.	
	registry ifNotNil: [
		registry deliver: announcement
		].
	^ announcement