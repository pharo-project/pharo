tests
testAnnouncerUnregistration
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #($a $b).
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #($d).
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #().
	self assert: announcer numberOfSubscriptions equals: 5.
	browser unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0