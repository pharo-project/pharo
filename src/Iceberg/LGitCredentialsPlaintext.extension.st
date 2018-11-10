Extension { #name : #LGitCredentialsPlaintext }

{ #category : #'*Iceberg-Adapters' }
LGitCredentialsPlaintext >> readFrom: credentials [
	self username: credentials username.
	self passphrase: credentials password.  

]
