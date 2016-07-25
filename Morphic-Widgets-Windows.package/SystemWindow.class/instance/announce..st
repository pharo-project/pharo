announcement
announce: anAnnouncement
	"announcer is lazy initialized. If announcer is not set, this means that there is no listener, no need to propagate the announcement therefore"
	self doAnnounce: anAnnouncement 