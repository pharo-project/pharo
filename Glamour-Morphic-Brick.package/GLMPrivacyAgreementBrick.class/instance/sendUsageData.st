preferences
sendUsageData
	| settingsClass |
	settingsClass := #Privacy asClassIfAbsent: [ ^ false ].
	^ settingsClass sendDiagnosticsAndUsageData