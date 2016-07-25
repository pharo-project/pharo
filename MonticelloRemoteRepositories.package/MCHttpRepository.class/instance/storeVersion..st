storing
storeVersion: aVersion
	self retryOnCredentialRequest: [ super storeVersion: aVersion ]