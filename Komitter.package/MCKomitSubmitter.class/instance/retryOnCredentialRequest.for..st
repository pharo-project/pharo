private
retryOnCredentialRequest: aBlock for: aRepository

	aBlock on: MCPermissionDenied do: [ :error | |credentials| 
		credentials := MCCredentialsRequest 
							signalUrl: aRepository location
							username: aRepository user
							password: aRepository password.
		credentials 
			ifNotNil: [ 
				aRepository credentials: credentials.
				^ self retryOnCredentialRequest: aBlock for: aRepository ]]