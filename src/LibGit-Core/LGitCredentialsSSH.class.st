"
I'm a set of credentials who asks public/private keys from from files (in case you need something different than what is answered by ssh-agent, provided by LGitCredentialsSSHAgent)
"
Class {
	#name : #LGitCredentialsSSH,
	#superclass : #LGitCredentials,
	#instVars : [
		'publicKeyReference',
		'privateKeyReference'
	],
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'instance creation' }
LGitCredentialsSSH class >> createFrom: provider onto: lgitCredentials [
	| credentials |
	
 	[
		credentials := self new.
		provider provideSshCredentialsTo: credentials.
		credentials createWith: lgitCredentials.
		^ 0
	]
	on: LGitNoCredentialsProvided do: [ 
		LGitError setError: 'Invalid ssh keys in provided files.'.		
		-1 ]

]

{ #category : #'instance creation' }
LGitCredentialsSSH class >> createFrom: provider username: username onto: lgitCredentials [
	 ^ self createFrom: provider onto: lgitCredentials
]

{ #category : #'instance creation' }
LGitCredentialsSSH class >> username: username passphrase: passphrase publicKeyReference: publicKeyFileReference privateKeyReference: privateKeyFileReference [
	^ self new
		username: username;
		passphrase: passphrase;
		publicKeyReference: publicKeyFileReference;
		privateKeyReference: privateKeyFileReference;
		yourself
]

{ #category : #operations }
LGitCredentialsSSH >> createWith: aStructure [
	<todo: 'use exceptions instead of assertions'>
	self assert: self username isEmptyOrNil not.
	self assert: self publicKeyReference exists.
	self assert: self privateKeyReference exists.

	self withReturnHandlerDo: [ 
		self 
			cred_ssh_key_new: aStructure
			username: self username
			publickey: self publicKeyReference fullName
			privatekey: self privateKeyReference fullName
			passphrase: self passphrase ]
]

{ #category : #'libgit-calls' }
LGitCredentialsSSH >> cred_ssh_key_new: out username: usernameString publickey: publickey privatekey: privatekey passphrase: passphraseString [
	^self call: 
		#(int git_cred_ssh_key_new(
			LGitCredentials * out, 
			String usernameString, 
			String publickey, 
			String privatekey, 
			String passphraseString)) options: #(  )
]

{ #category : #'libgit-calls' }
LGitCredentialsSSH >> cred_username_new: out username: usernameString [
	^ self 
		call: #(int git_cred_username_new (LGitCredentials * out, String usernameString)) 
		options: #()
			
]

{ #category : #'libgit-fields' }
LGitCredentialsSSH >> prim_credtype [
	"This method was automatically generated"
	^LGitCredentialsTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_CREDTYPE)
]

{ #category : #'libgit-fields' }
LGitCredentialsSSH >> prim_credtype: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CREDTYPE put: anObject value
]

{ #category : #'libgit-fields' }
LGitCredentialsSSH >> prim_free [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_FREE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCredentialsSSH >> prim_free: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FREE put: anObject getHandle.
]

{ #category : #accessing }
LGitCredentialsSSH >> privateKeyReference [
	^ privateKeyReference
]

{ #category : #accessing }
LGitCredentialsSSH >> privateKeyReference: aFileReference [
	privateKeyReference := aFileReference
]

{ #category : #accessing }
LGitCredentialsSSH >> publicKeyReference [
	^ publicKeyReference
]

{ #category : #accessing }
LGitCredentialsSSH >> publicKeyReference: aFileReference [
	publicKeyReference := aFileReference
]

{ #category : #operations }
LGitCredentialsSSH >> setGitUsernameIn: aStructure [
	self withReturnHandlerDo: [ 
		self cred_username_new: aStructure username: 'git'
	]
]
