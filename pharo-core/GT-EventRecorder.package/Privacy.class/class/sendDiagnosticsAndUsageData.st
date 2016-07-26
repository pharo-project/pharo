accessing
sendDiagnosticsAndUsageData
	^ sendDiagnosticsAndUsageData ifNil: [ sendDiagnosticsAndUsageData := false ]