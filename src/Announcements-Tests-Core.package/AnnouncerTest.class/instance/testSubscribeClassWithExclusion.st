tests - subscribing
testSubscribeClassWithExclusion
	| announcement instance |
	announcer
		subscribe: AnnouncementMockB - AnnouncementMockC
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement = instance.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement isNil