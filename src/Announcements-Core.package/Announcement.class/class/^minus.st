public
- anotherAnnouncement
	"Create an announcement set containing the receiver 
	and anotherAnnouncement as exclusion."

	^ AnnouncementSetWithExclusions new
		add: self;
		addExclusion: anotherAnnouncement;
		yourself