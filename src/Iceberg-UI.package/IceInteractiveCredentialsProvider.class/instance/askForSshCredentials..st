actions
askForSshCredentials: response
	| model cancelled |
	
	cancelled := false.
	(model := IceAskForSshCredentialsModel new)
		openDialogWithSpec
		okAction: [
			| credentials | 
			credentials := model credentials.
			model storeCredentials 
				ifTrue: [ self class sshCredentials: credentials ].
			response readFrom: credentials ];
		cancelAction: [ cancelled := true ];
		centered;
		modalRelativeTo: World.

	cancelled ifTrue: [ LGitNoCredentialsProvided signal ]
