tests
testSimpleNestedSuspendParticularAnnouncementWhile
	| announcement |
	announcer
		when: GLMTestAnnouncement
		do: [ :ann | announcement := ann ].
	
	announcement := nil.
	announcer suspendAll: {GLMTestAnnouncement} while: [
		announcer suspendAll: {} while: [ ].
		announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil