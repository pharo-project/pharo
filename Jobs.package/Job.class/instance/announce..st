accessing
announce: anAnnouncementClass

	| announcement |
	announcement := anAnnouncementClass on: self.
	self announcer announce: announcement.