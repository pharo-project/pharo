private
guessUsernameOrEmailNotFound: anError ifNot: notBlock 
	(anError messageText = 'Config value ''user.name'' was not found' 
		or: [ anError messageText = 'Config value ''user.email'' was not found' ])
		ifTrue: [ IceGitUsernameOrEmailNotFound signal ].
	notBlock value