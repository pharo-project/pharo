repositories
password
	| password |
	password := target configurationRoot password. 
	password ifNotNil: [ ^ password ].
	password := self uiManager requestPassword: 'enter your Monticello password for '.
	target configurationRoot password: password.
	^ password