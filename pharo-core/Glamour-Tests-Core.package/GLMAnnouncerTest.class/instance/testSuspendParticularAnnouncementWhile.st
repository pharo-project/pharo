tests
testSuspendParticularAnnouncementWhile
	| announcement instance |
	announcer when: GLMTestAnnouncement do: [ :ann | announcement := ann ].
	announcer when: GLMTestAnnouncement2 do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: GLMTestAnnouncement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: Announcement ].
	self assert: announcement isNil.
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [ instance := announcer announce: GLMTestAnnouncement2 ].
	self assert: announcement equals: instance.