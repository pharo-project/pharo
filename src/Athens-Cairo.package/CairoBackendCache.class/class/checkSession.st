session management
checkSession
	session == Smalltalk session ifFalse: [ 
		soleInstance := nil.
		session := Smalltalk session ]