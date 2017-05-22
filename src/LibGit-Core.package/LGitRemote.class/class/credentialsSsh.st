convenience
credentialsSsh
	^ LGitCredentialsSSH
		username: self usernameSsh
		passphrase: self keyPassPhrase
		publicKeyReference: self keyFilePublicPath asFileReference
		privateKeyReference: self keyFilePrivatePath asFileReference