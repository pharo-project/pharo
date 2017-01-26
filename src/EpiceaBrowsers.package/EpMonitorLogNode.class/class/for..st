instance creation
for: anEpMonitor

	self flag: #fix.
	
	^ self new
		theLog: anEpMonitor basicLog;
		headReference: anEpMonitor log headReference; 
			"note: this is a workaround given this information is needed to build the references graph,
			but it's not available from 'theLog' when it's empty"
		yourself
