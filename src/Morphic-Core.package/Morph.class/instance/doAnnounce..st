announcements
doAnnounce: anAnnouncement
	"Take care of not creating the announcer when announcing. If the announcer doesn't exist then this means nobody has expressed an interest in the message."

	"Do not override announce: for now, there is a need to refactor the announcements code in at least SystemWindow and ExpanderMorph."

	(self valueOfProperty: #announcer ifAbsent: [ ^ self ]) announce: anAnnouncement