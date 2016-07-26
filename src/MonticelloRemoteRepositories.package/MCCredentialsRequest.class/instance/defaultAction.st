exceptiondescription
defaultAction
	
	username := UIManager default
			request: self userMessagePrompt
			initialAnswer: self username
			title: 'User' translated.
	(username isNil or: [ username isEmpty ])
		ifTrue: [ ^ nil ].
	password := UIManager default
			requestPassword: self passwordMessagePrompt.
	^ MCServerCredentials user: username password: password