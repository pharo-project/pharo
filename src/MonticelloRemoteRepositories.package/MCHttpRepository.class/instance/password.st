actions
password
	self userAndPasswordFromSettingsDo: [:usr :pwd | ^pwd].

	self user isEmpty ifTrue: [^password ifNil: ['']].

	password isEmptyOrNil ifTrue: [
		| answer |
		user isEmptyOrNil ifTrue: [
			"Give the user a chance to change the login"
			answer := UIManager default request: 'User name for ', String cr, location
				initialAnswer: ''.
			answer isEmptyOrNil
				ifTrue: [^password]
				ifFalse: [self user: answer] ].
		
		password := UIManager default requestPassword: 'Password for "', self user, '" at ', String cr, self location.
		"The user doesn't care about logging into this repo. Clear the username so they will not be prompted every time"
		password isEmptyOrNil ifTrue: [ user := '' ].
	].

	^ password