exclusion
- anotherAnnouncement
	"Create an announcement set containnig the receiver
	and anotherAnnouncement as an exclusion."

	^ AnnouncementSetWithExclusions new
		add: self;
		addExclusion: anotherAnnouncement;
		yourself