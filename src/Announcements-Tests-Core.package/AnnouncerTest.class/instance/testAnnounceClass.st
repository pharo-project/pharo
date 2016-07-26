testing
testAnnounceClass
	self assert: (announcer announce: AnnouncementMockA) 
		class = AnnouncementMockA