"
I indicate when an error trying to authenticate occurs (tipically, when cloning without credentials)
"
Class {
	#name : #IceAuthenticationError,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceAuthenticationError >> acceptError: aVisitor [
	aVisitor visitAuthenticationError: self
]

{ #category : #accessing }
IceAuthenticationError >> defaultMessageText [
	^ 'There was an authentication error while trying to execute the operation: {1}. 
This happens usually because you didn''t provide a valid set of credentials. 
You may fix this problem in different ways: 

1. adding your keys to ssh-agent, executing ssh-add ~/.ssh/id_rsa in your command line.
2. adding your keys in settings (open settings browser search for "Use custom SSH keys" and
add your public and private keys).
3. using HTTPS instead SSH (Just use an url in the form HTTPS://etc.git)'
]

{ #category : #accessing }
IceAuthenticationError >> messageText [
	^ messageText ifNil: [self defaultMessageText format: { super messageText }]
]
