Class {
	#name : #LGitCredentialsPlaintext,
	#superclass : #LGitCredentials,
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'instance creation' }
LGitCredentialsPlaintext class >> createFrom: provider onto: lgitCredentials [
	| credentials |
	[
		credentials := self new.
		provider providePlaintextCredentialsTo: credentials.
		credentials createWith: lgitCredentials.
		^ 0
	]
	on: LGitNoCredentialsProvided do: [ :e | 
		LGitError setError: 'Invalid username and/or password provided.'.				
		^ -1 ]

]

{ #category : #'instance creation' }
LGitCredentialsPlaintext class >> username: username passphrase: passphrase [
	^ self new
		username: username;
		passphrase: passphrase;
		yourself
]

{ #category : #operations }
LGitCredentialsPlaintext >> createWith: aStructure [
	<todo: 'use exceptions instead of assertions'>
	self assert: self username isEmptyOrNil not.
	self assert: self passphrase isEmptyOrNil not.

	self withReturnHandlerDo: [ 
		self 
			cred_userpass_plaintext_new: aStructure
			username: self username
			password: self passphrase ]
]

{ #category : #'libgit-calls' }
LGitCredentialsPlaintext >> cred_userpass_plaintext_new: out username: usernameString password: password [ 
	^self call: #(
		LGitReturnCodeEnum git_cred_userpass_plaintext_new(
			LGitCredentials * out, 
			String username, 
			String password)
	) options: #(  )
]

{ #category : #'libgit-fields' }
LGitCredentialsPlaintext >> prim_credtype [
	"This method was automatically generated"
	^LGitCredentialsTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_CREDTYPE)
]

{ #category : #'libgit-fields' }
LGitCredentialsPlaintext >> prim_credtype: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CREDTYPE put: anObject value
]

{ #category : #'libgit-fields' }
LGitCredentialsPlaintext >> prim_free [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_FREE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCredentialsPlaintext >> prim_free: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FREE put: anObject getHandle.
]
