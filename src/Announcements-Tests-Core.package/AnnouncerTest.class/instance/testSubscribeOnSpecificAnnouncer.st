tests - subscribing
testSubscribeOnSpecificAnnouncer
	| announcement instance |
	announcer
		subscribe: (AnnouncementMockB where: [:ann | ann parameter = #expectedParam] )
		do: [ :ann | announcement := ann ].
		
	announcement := nil.
	instance := announcer announce: AnnouncementMockA.
	self assert: announcement equals: nil.

	announcement := nil.
	instance := announcer announce: AnnouncementMockB.
	self assert: announcement equals: nil.

	announcement := nil.
	instance := announcer announce: (AnnouncementMockB with: #wrongParam).
	self assert: announcement equals: nil.

	announcement := nil.
	instance := announcer announce: (AnnouncementMockB with: #expectedParam).
	self assert: announcement = instance.
	
	announcement := nil.
	instance := announcer announce: (AnnouncementMockC with: #expectedParam).
	self assert: announcement equals: instance.