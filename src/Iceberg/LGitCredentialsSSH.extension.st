Extension { #name : #LGitCredentialsSSH }

{ #category : #'*Iceberg-Adapters' }
LGitCredentialsSSH >> readFrom: credentials [
	self username: credentials username.
	self passphrase: credentials keyPassphrase.  
	self publicKeyReference: credentials publicKey asFileReference.
	self privateKeyReference: credentials privateKey asFileReference
]
