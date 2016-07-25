tests
testAnnouncementClassRemovedIsRaisedOnRemoveFromSystem
	"self debug: #testAnnouncementClassRemovedIsRaisedOnRemoveFromSystem"
	| foo | 
	
	[ notRun := false.
	  SystemAnnouncer uniqueInstance 
		when: ClassRemoved send: #setNotRun to: self.
	foo := self createNewClassNamed: #FooForTest2.
	self deny: notRun.
	foo removeFromSystem.
	self assert: notRun ]
ensure: [ SystemAnnouncer uniqueInstance unsubscribe: self ]. 