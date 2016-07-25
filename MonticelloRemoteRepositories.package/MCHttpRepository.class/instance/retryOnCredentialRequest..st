actions
retryOnCredentialRequest: aBlock
	aBlock	on: MCPermissionDenied do: [ :error | |credentials| 
		credentials := MCCredentialsRequest 
							signalUrl: self location
							username: self user
							password: self password.
		credentials 
			ifNotNil: [ 
				self credentials: credentials.
				^ self retryOnCredentialRequest: aBlock ]]