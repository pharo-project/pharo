events
recordAnnouncement: anAnnouncement
	Privacy sendDiagnosticsAndUsageData ifFalse: [ ^ self ].	
	(anAnnouncement respondsTo: #dataForQAR) ifTrue: [
		[ self add:
			(anAnnouncement dataForQAR
				at: #obfuscated put: Privacy sendEntityNames not;
				at: #timestamp  put: DateAndTime now;
				yourself)
		
		] on: Error
		  do: [ :error |
				ReSettings showErrors ifTrue: [ error pass ] ] ]