private
newAnnouncementFor: aGTEventCollector
	| announcement |
	announcement := self 
		updateAnnouncement: self newAnnouncement 
		for: aGTEventCollector.
	aGTEventCollector updateAnnouncement: announcement.
	^ announcement