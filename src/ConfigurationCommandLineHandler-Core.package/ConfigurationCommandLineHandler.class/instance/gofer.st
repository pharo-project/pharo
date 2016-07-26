accessing
gofer
	| username password |
	username := (self hasOption: 'username')
		ifTrue: [ self optionAt: 'username' ]
		ifFalse: [ String new ].
	password := (self hasOption: 'password')
		ifTrue: [ self optionAt: 'password' ]
		ifFalse: [ String new ].
	^ Gofer new
		url: self repositoryUrl username: username password: password;
		yourself