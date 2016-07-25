events
recordSpotterOpenEvent
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	self add: GTSpotterRecorderOpenEvent new