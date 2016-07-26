adding
addIfAvailable: anEvent
	"It add event to a queue only if sending diagnostic and usage data is enabled."
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ anEvent ].	
	^ self add: anEvent