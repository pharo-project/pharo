running
allLogEntriesWith: aClass
	
	self flag: #todo. "This is too hidden here, but important!"	
	monitor sessionStore flush.
	
	^ monitor log entries select: [ :each | each content class = aClass ]