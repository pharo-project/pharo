subscription
announce: anAnnouncement
	"see Announcements packages"

	self announcing ifTrue:	[ self announcer announce: anAnnouncement ]