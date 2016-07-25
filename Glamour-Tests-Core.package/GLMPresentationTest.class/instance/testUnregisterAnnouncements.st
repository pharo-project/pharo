tests
testUnregisterAnnouncements
	| announcer presentation |
	announcer := Announcer new.
	presentation := GLMPresentation new.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	presentation registerAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 1.
	presentation unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0