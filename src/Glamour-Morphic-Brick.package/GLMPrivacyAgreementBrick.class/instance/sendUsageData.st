preferences
sendUsageData	
	^ self class environment at: #Privacy 
		ifPresent: [ :privacy | privacy sendDiagnosticsAndUsageData ]
		ifAbsent: [ false ].
		