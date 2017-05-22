instance creation
username: username passphrase: passphrase publicKeyReference: publicKeyFileReference privateKeyReference: privateKeyFileReference
	^ self new
		username: username;
		passphrase: passphrase;
		publicKeyReference: publicKeyFileReference;
		privateKeyReference: privateKeyFileReference;
		yourself