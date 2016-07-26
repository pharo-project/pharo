events
recordAnnouncement: anAnnouncement
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	(anAnnouncement respondsTo: #asRecorderEvent) ifTrue: [	
		self add: anAnnouncement asRecorderEvent ]
