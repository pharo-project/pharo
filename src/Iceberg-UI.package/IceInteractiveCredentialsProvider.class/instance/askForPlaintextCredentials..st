actions
askForPlaintextCredentials: response
	| model cancelled |
	
	cancelled := false.
	(model := IceAskForPlaintextCredentialsModel new)
		openDialogWithSpec
		okAction: [
			| credentials | 
			credentials := model credentials.

			model storeCredentials 
				ifTrue: [ self class plaintextCredentials: credentials ].

			response readFrom: credentials.
		];
		cancelAction: [ cancelled := true ];
		centered;
		modalRelativeTo: World.

	cancelled ifTrue: [ LGitNoCredentialsProvided signal ]