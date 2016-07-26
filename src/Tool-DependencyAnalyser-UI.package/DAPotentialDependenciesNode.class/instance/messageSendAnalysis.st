accessing
messageSendAnalysis
	messageSendAnalysis ifNil: [ messageSendAnalysis := DAMessageSendAnalyzer on: self parentNode content packageName ].
	^ messageSendAnalysis