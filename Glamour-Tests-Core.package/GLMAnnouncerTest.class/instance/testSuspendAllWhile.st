tests
testSuspendAllWhile
	| announcement instance |
	announcer when: GLMTestAnnouncement do: [ :ann | announcement := ann ].
	announcement := nil.
	instance := announcer announce: GLMTestAnnouncement.
	self assert: announcement equals: instance.
	announcement := nil.
	announcer suspendAllWhile: [ instance := announcer announce: GLMTestAnnouncement ].
	self assert: announcement isNil