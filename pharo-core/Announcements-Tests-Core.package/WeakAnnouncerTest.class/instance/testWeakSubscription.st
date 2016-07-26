testing
testWeakSubscription
	| obj  subscription |
	
	obj := Object new.
	
	subscription := 
		(announcer when: AnnouncementMockA send: #value to: obj) makeWeak.
		
	self assert: (obj == subscription subscriber).