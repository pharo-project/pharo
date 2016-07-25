tests
testNestedSuspendParticularAnnouncementWhile
	| announcement instance2 announcement2 |
	announcer
		when: GLMTestAnnouncement, GLMTestAnnouncement2
		do: [ :ann | announcement := ann ].
	
	announcement2 := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [
		announcer suspendAll: {GLMTestAnnouncement2} while: [  
			instance2 := announcer announce: GLMTestAnnouncement.
			announcement2 := announcement.
			announcement := nil. ].
		announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil.
	self assert: announcement2 equals: instance2.