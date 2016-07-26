repositories
username
	| username |
	username := target configurationRoot username. 
	username ifNotNil: [ ^ username ].
	username := self uiManager request: 'enter your Monticello username (login)'.
	target configurationRoot username: username.
	^ username