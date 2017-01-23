accessing
monitor
	
	^ monitor ifNil: [ monitor := EpMonitor current ]