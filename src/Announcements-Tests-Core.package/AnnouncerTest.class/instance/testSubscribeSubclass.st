tests - subscribing
testSubscribeSubclass
	| announcement instance |
	announcer
		subscribe: AnnouncementMockB
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.

	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement = instance.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement = instance.