testing
testAnnounceInstance
	| instance |
	instance := AnnouncementMockA new.
	self assert: (announcer announce: instance) = instance