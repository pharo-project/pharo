running
allLogEntriesWith: aClass

	^ monitor log entries select: [ :each | each content class = aClass ]