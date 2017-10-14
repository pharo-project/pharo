tests - subscribing
testSubscribeSetWithExclusionOfSetItem
	| announcement instance |
	announcer
		subscribe: AnnouncementMockA , AnnouncementMockC - AnnouncementMockA
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement isNil.
	
	announcement := nil.
	instance := announcer announce: AnnouncementMockC.
	self assert: announcement = instance