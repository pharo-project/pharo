ui requests
confirm: queryString
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^ answer].
	
	^  self nonInteractiveWarning: 'Confirming: ', queryString