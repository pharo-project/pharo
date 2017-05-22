private requesting
withResponseValidationDo: aBlock
	| response |
	
	response := aBlock value.
	response isSuccess 
		ifFalse: [ IceGitHubError signalFor: response ].
	^ response contents