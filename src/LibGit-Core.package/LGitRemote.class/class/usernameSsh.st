accessing-credentials
usernameSsh
	^ self credentials
		at: #usernameSsh
		ifAbsentPut: [ 
			UIManager default 
				request: 'What''s your username for SSH (probably "git")?'
				initialAnswer: 'git' ]