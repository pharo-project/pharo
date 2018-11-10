"
I'm a set of credentials who feeds from the ssh-agent client.
"
Class {
	#name : #LGitCredentialsSSHAgent,
	#superclass : #LGitCredentials,
	#category : #'LibGit-Core-FFI-Structs'
}

{ #category : #'instance creation' }
LGitCredentialsSSHAgent class >> createFrom: provider username: username onto: lgitCredentials [
	| credentials |
	
	[  
		credentials := self new.
		provider provideSshAgentCredentialsTo: credentials.	
		credentials createWith: username onto: lgitCredentials.
		^ 0 ]
	on: LGitNoCredentialsProvided do: [ 
		LGitError setError: 'No ssh-agent suitable credentials found.'.
		^ -1 ]
]

{ #category : #operations }
LGitCredentialsSSHAgent >> createWith: aName onto: aStructure [
	<todo: 'use exceptions instead of assertions'>
	self assert: aName isEmptyOrNil not.

	self withReturnHandlerDo: [ 
		self cred_ssh_key_new: aStructure username: aName ]
]

{ #category : #'libgit-calls' }
LGitCredentialsSSHAgent >> cred_ssh_key_new: out username: aName [
	^ self call: #(int git_cred_ssh_key_from_agent(LGitCredentials *out, String aName))

]

{ #category : #'libgit-fields' }
LGitCredentialsSSHAgent >> prim_credtype [
	"This method was automatically generated"
	^LGitCredentialsTypeEnum fromInteger: (handle unsignedLongAt: OFFSET_PRIM_CREDTYPE)
]

{ #category : #'libgit-fields' }
LGitCredentialsSSHAgent >> prim_credtype: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_PRIM_CREDTYPE put: anObject value
]

{ #category : #'libgit-fields' }
LGitCredentialsSSHAgent >> prim_free [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PRIM_FREE) type: ExternalType void asPointerType
]

{ #category : #'libgit-fields' }
LGitCredentialsSSHAgent >> prim_free: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PRIM_FREE put: anObject getHandle.
]
