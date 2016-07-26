session management
checkSession
	session == Smalltalk session ifFalse: [ 
		self recreateOSWindow.
		session := Smalltalk session.
	]