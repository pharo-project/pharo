instance creation
openWithLog: aLog

	^ (self newWithLog: aLog)
		refresh;
		open;
		yourself