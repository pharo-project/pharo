ui requests
confirm: queryString

	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer |
		^answer].

	self error: 'No user response possible'