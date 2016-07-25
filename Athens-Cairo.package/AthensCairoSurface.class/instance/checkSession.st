private
checkSession
	session == Smalltalk session ifFalse: [ 
		self error: 'Attempt to use invalid external resource (left from previous session)' ]