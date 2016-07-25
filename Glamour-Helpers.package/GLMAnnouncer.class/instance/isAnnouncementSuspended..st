testing
isAnnouncementSuspended: anAnnouncement
	^ self suspendAll or: [ |announcementType|
		announcementType := anAnnouncement isClass 
			ifTrue: [ anAnnouncement ] ifFalse: [ anAnnouncement class ].
		self suspendedAnnouncemets anySatisfy: [ :anAnnouncementType |
			"Suspend the announcement if it has the same class or if it inherits from a suspended announcement class."
			announcementType = anAnnouncementType or: [ announcementType  inheritsFrom: anAnnouncementType ]  ] ]