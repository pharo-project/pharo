tests
testSuspendParticularAnnouncementWithInheritanceWhile
	| announcement instance |
	announcer when: Announcement do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: Announcement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAll: {Announcement} while: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil