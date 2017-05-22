actions
provideSshCredentialsTo: credentials
	tries := tries + 1.

	tries >= 3 ifTrue: [ 
		Transcript show: 'aborting'; cr.
		LGitNoCredentialsProvided signal: 'There where already three failed attempts, aborting' ].
	
	tries = 1 & self sshCredentials isPresent
		ifTrue: [ credentials readFrom: self sshCredentials ]
		ifFalse: [ self askForSshCredentials: credentials ].