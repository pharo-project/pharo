private
retryOnCredentialRequest: aBlock
	aBlock on: MCPermissionDenied do: [ :e | 
		| credentials remoteRepository | 
	
		remoteRepository := self remoteRepository.
		credentials := MCCredentialsRequest 
			signalUrl: remoteRepository location
			username: remoteRepository user
			password: remoteRepository password.
		credentials ifNotNil: [
			remoteRepository credentials: credentials.
			^ self retryOnCredentialRequest: aBlock ] ]