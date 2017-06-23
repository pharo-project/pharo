private
configGitUsernameAndEmailOnCancel: cancelBlock
	| passed |
	passed := false.
	(IceAskForUsernameAndEmailModel new 
		onAccept: [ :username :email :global |
			self repository 
				username: username
				email: email
				global: global.
			passed := true ];	
		openWithSpec)
		modalRelativeTo: World.
		
	passed ifFalse: cancelBlock