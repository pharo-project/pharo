repositories
checkUserInRepository: repository
	"Try to find the http repository of packageName. If it does not find one, then add one and ask for the username and password"

	| answerUser |
	(repository isKindOf: MCHttpRepository ) ifFalse: [ ^self ].
	repository user
		ifEmpty: [ 
			answerUser := self uiManager confirm: 'No user defined. Do you want to add one? '.
			answerUser
				ifTrue: [ 
					repository user: self username.
					repository password: self password ] ]