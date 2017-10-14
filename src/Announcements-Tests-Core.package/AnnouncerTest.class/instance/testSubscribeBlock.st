tests - subscribing
testSubscribeBlock
	| announcement instance |
	announcer
		subscribe: AnnouncementMockA
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement = instance.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement isNil