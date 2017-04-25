announcement
packageSelected: anAnnouncement
	
	| package |
	package := anAnnouncement package package.
	package
		ifNil: [ tasks removeAll ]
		ifNotNil: [ tasks := (self systemNavigation allCallsOn: #flag:) 
				select: [ :m | package includesMethod: m selector ofClass: m methodClass ] ].
	
	index := 0.	
	self changed: #tasks