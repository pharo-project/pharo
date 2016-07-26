session management
checkSession

	session == Smalltalk session ifFalse: [ 
		self error: 'Cannot use this renderer (created in a previous session or in a closed window)'.
	].