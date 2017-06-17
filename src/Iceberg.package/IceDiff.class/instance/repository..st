accessing
repository: anObject
	repository := anObject.
	repository announcer weak
		subscribe: IceCommited send: #refresh to: self