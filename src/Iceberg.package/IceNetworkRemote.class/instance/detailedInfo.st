accessing
detailedInfo
	^ { 
		'Origin' -> self url.
		'Remote User' -> self userName.
		'Remote host' -> (self port = self class defaultPort ifTrue: [ self host ] ifFalse: [self host, ':', self portName]).
		'Repo Owner' -> self owner.
	} 