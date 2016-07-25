accessing
sendDiagnosticsAndUsageData: aBoolean
	sendDiagnosticsAndUsageData = aBoolean ifTrue: [ ^ self ].
	sendDiagnosticsAndUsageData := aBoolean.
	SystemAnnouncer uniqueInstance announce: PrivacyChangedAnnoucement.