"
I am a container for the credentials needed for connecting to a git repository via ssh/scp.
"
Class {
	#name : #IceSshCredentials,
	#superclass : #IceAbstractCredentials,
	#instVars : [
		'username',
		'publicKey',
		'privateKey',
		'keyPassphrase'
	],
	#category : #'Iceberg-Security'
}

{ #category : #accessing }
IceSshCredentials class >> defaultPrivateKey [ 	
	self flag: #todo. "In fact this is different for windows. We need to change this."
	^ (FileLocator home asFileReference / '.ssh' / 'id_rsa') pathString
]

{ #category : #accessing }
IceSshCredentials class >> defaultPublicKey [
	self flag: #todo. "In fact this is different for windows. We need to change this."
	^ (FileLocator home asFileReference / '.ssh' / 'id_rsa.pub') pathString
]

{ #category : #accessing }
IceSshCredentials class >> defaultUsername [
	^ 'git'
]

{ #category : #printing }
IceSshCredentials >> description [

	^ 'User: ' , self username , ' Public: ' , self publicKey , ' Private: ' , self privateKey 
]

{ #category : #printing }
IceSshCredentials >> hostDescription [
	^ host = #'*' ifTrue: [ 'Default' ] ifFalse: [ host ]
]

{ #category : #testing }
IceSshCredentials >> isDefault [

	^ host = #'*'
]

{ #category : #testing }
IceSshCredentials >> isPresent [
	^ self username notEmpty 
		and: [ self publicKey notEmpty 
		and: [ self privateKey notEmpty ] ]
]

{ #category : #accessing }
IceSshCredentials >> keyPassphrase [
	^ keyPassphrase ifNil: [ keyPassphrase := '' ]
]

{ #category : #accessing }
IceSshCredentials >> keyPassphrase: anObject [
	keyPassphrase := anObject.
	
	self storeIfDefault.
]

{ #category : #accessing }
IceSshCredentials >> privateKey [
	^ privateKey ifNil: [ privateKey := self class defaultPrivateKey ]
]

{ #category : #accessing }
IceSshCredentials >> privateKey: anObject [
	privateKey := anObject.
	
	self storeIfDefault.
]

{ #category : #accessing }
IceSshCredentials >> publicKey [
	^ publicKey ifNil: [ publicKey := self class defaultPublicKey ]
]

{ #category : #accessing }
IceSshCredentials >> publicKey: anObject [
	publicKey := anObject.
	
	self storeIfDefault.
]

{ #category : #'private - storing' }
IceSshCredentials >> removeFrom: aStore [ 

	aStore removeSSHCredential: self.
]

{ #category : #'private - storing' }
IceSshCredentials >> storeIfDefault [
	
	self isDefault ifFalse: [ ^ self ].
	IceCredentialStore current storeCredential: self.
]

{ #category : #'API - storing' }
IceSshCredentials >> storeInto: aCredentialStore forHostname: aHost [

	host := aHost.
 	aCredentialStore storeSshCredential: self forHostname: aHost
]

{ #category : #accessing }
IceSshCredentials >> type [
	^ 'SSH Keys'
]

{ #category : #accessing }
IceSshCredentials >> username [
	^ username ifNil: [ username := self class defaultUsername ]
]

{ #category : #accessing }
IceSshCredentials >> username: anObject [
	username := anObject
]
