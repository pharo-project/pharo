accessing
messageSendAnalysis
	[ messageSendAnalysis ifNil: [ messageSendAnalysis := DAMessageSendAnalyzer on: self parentNode content packageName ] ]
		on: PotentialOutDatedDependencyWarning 
		do: [ :ex | ex resume. "ignore. We already got an alert box" ].
	^ messageSendAnalysis