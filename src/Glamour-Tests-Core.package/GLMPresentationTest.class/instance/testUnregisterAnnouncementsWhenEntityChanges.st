tests
testUnregisterAnnouncementsWhenEntityChanges
	| announcer1 presentation |
	announcer1 := Announcer new.
	presentation := GLMPresentation new.
	presentation updateOn: GLMTestAnnouncement from: [ announcer1 ].
	presentation entity: 1.
	presentation registerAnnouncements.
	self assert: announcer1 numberOfSubscriptions equals: 1.
	presentation entity: 2.
	self assert: announcer1 numberOfSubscriptions equals: 0.
	presentation registerAnnouncements.
	self assert: announcer1 numberOfSubscriptions equals: 1.
	presentation entity: nil.
	self assert: announcer1 numberOfSubscriptions equals: 0