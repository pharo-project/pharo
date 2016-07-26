private
storeDependencies: newVersion for: aKomit

	self 
		retryOnCredentialRequest: [ aKomit repository storeDependencies: newVersion ]
		for: aKomit repository