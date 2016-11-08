ui requests
confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption
	(ProvideAnswerNotification signal: queryString) ifNotNil: [:answer | ^ answer].

	self error: 'No user response possible'