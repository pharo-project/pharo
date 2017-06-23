*Iceberg-Adapters
readFrom: credentials
	self username: credentials username.
	self passphrase: credentials keyPassphrase.  
	self publicKeyReference: credentials publicKey asFileReference.
	self privateKeyReference: credentials privateKey asFileReference