private
storeVersion: newVersion for: aKomit

	self 
		retryOnCredentialRequest: [ aKomit repository storeVersion: newVersion ]
		for: aKomit repository