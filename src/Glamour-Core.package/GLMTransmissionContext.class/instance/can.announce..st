accessing
can: aPane announce: anAnnouncement
	^ ((self forbiddenAnnouncements at: aPane ifAbsent: [Set new])
		includes: anAnnouncement ) not