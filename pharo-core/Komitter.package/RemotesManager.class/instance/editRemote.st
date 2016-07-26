private
editRemote
	| item view |
	
	item := remotes highlightedItem content.
	view := CredentialEditor new.
	view 
		setPassword: item password;
		setUsername: item username.
	
	view openDialogWithSpec
		centered;
		modalRelativeTo: self window.
	
	view cancelled ifTrue: [ ^ self ].
	
	item 
		username: view getUsername;
		password: view getPassword