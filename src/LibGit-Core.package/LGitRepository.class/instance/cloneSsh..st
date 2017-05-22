operations
cloneSsh: aString
	| credentials cloneOptions |
	credentials := LGitRemote credentialsSsh.
	cloneOptions := LGitCloneOptions withCredentialsCallback: 
		[ :cred :url :username_from_url :allowed_types :data | 
			allowed_types = 32 
				ifTrue: [ credentials setGitUsernameIn: cred ]
				ifFalse: [ credentials createWith: cred ].
			0 
		].
	
	self
		clone: aString
		options: cloneOptions	