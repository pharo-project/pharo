"
I am the responsible for storing the credentials. 
Now I can store plain text credentials and SSH key pairs. 

I store the credentials in a fuel file, this is non encrypted. So it should be improved later.

There is a default instance that stores the credentials in a file per image, in the pharo-local directory.
In the settings you can modify the location of the credentials file.
"
Class {
	#name : #IceCredentialStore,
	#superclass : #Object,
	#instVars : [
		'plainCredentials',
		'sshCredentials',
		'storeFile'
	],
	#classVars : [
		'CurrentStore',
		'StoreFile'
	],
	#category : #'Iceberg-Security'
}

{ #category : #accessing }
IceCredentialStore class >> current [
	^ CurrentStore
		ifNil: [ CurrentStore := self new
				storeFile: self storeFile;
				yourself ]
]

{ #category : #accessing }
IceCredentialStore class >> settingsOn: aBuilder [
	<systemsettings>
		
	(aBuilder group: #PlainTextCredentials)
		parent: #Iceberg;
		name: #icebergCredentials;
		noOrdering;
		target: self current;
		label: 'Plain Text Credentials';
		with: [
			(aBuilder setting: #storeFile)
				target: self;
				type: #FilePathEncoder;
				label: 'Location of credentials store';
				description: 'The location of the credential store. The password are stored in plain... yet'.
		].	

]

{ #category : #accessing }
IceCredentialStore class >> storeFile [
	^ StoreFile ifNil: [ StoreFile := FileLocator localDirectory / #iceberg / 'credentials.fuel' ]
]

{ #category : #accessing }
IceCredentialStore class >> storeFile: aValue [
	StoreFile := aValue
]

{ #category : #'API - accessing' }
IceCredentialStore >> allCredentials [
	self loadFromStore.
	self defaultSSHCredential.
	
	^ plainCredentials values , sshCredentials values
]

{ #category : #'API -  accessing' }
IceCredentialStore >> defaultSSHCredential [
	^ sshCredentials
		at: #*
		ifAbsentPut: [ IceSshCredentials new
				host: #*;
				yourself ]
]

{ #category : #initialization }
IceCredentialStore >> initialize [

	super initialize.
	plainCredentials := Dictionary new.
	sshCredentials := Dictionary new.
]

{ #category : #testing }
IceCredentialStore >> isCurrent [
	^ self == CurrentStore
]

{ #category : #'API - storing' }
IceCredentialStore >> loadFromStore [

	| array |
	self flag: #implementAnEncryptedVersion.
	
	"If I don't have a store file I do nothing"
	storeFile ifNil: [ ^ self ].
	
	"If the file is new, I cannot load it."
	storeFile exists ifFalse: [ ^ self ].
	
	array := FLMaterializer materializeFromFileNamed: storeFile resolve pathString.
	plainCredentials := array at: 1.
	sshCredentials := array at: 2.

]

{ #category : #'private - lookup' }
IceCredentialStore >> lookup: aHostName in: dict ifAbsent: aBlock [

	self loadFromStore.
	^ dict at: aHostName ifAbsent: aBlock
]

{ #category : #'API -  accessing' }
IceCredentialStore >> plaintextCredentialForHostname: aHostname [
 
	^ self lookup: aHostname in: plainCredentials ifAbsent: [ IcePlaintextCredentials new ]
]

{ #category : #'API - storing' }
IceCredentialStore >> removeCredential: aCredential [ 

	aCredential removeFrom: self.
	self saveIntoStore.
]

{ #category : #'private - storing' }
IceCredentialStore >> removePlainTextCredential: anIcePlaintextCredentials [ 
	
	plainCredentials removeKey: anIcePlaintextCredentials host
]

{ #category : #'private - storing' }
IceCredentialStore >> removeSSHCredential: aCredential [ 
	
	sshCredentials removeKey: aCredential host
]

{ #category : #'API - storing' }
IceCredentialStore >> saveIntoStore [

	| serializer |
	
	self flag: #implementAnEncryptedVersion.
		
	storeFile ifNotNil: [  	
		storeFile parent ensureCreateDirectory.	
		
		storeFile resolve binaryWriteStreamDo: [ :stream | 
			serializer := FLSerializer on:  stream.
			serializer serialize: {plainCredentials. sshCredentials}]
	]
]

{ #category : #'API -  accessing' }
IceCredentialStore >> sshCredentialForHostname: aHostname [
 
	^ self lookup: aHostname in: sshCredentials ifAbsent: [ self defaultSSHCredential ].
]

{ #category : #'API -  accessing' }
IceCredentialStore >> storeCredential: credential [
 
	self loadFromStore.
	credential storeInto: self forHostname: credential host.
	self saveIntoStore
]

{ #category : #'API -  accessing' }
IceCredentialStore >> storeCredential: credential forHostname: aHostname [
 
	self loadFromStore.
	credential storeInto: self forHostname: aHostname.
	self saveIntoStore
]

{ #category : #accessor }
IceCredentialStore >> storeFile: aFileLocator [ 
	storeFile := aFileLocator
]

{ #category : #'private - storing' }
IceCredentialStore >> storePlaintextCredential: credential forHostname: aHostname [
 
	plainCredentials at: aHostname put: credential
]

{ #category : #'private - storing' }
IceCredentialStore >> storeSshCredential: credential forHostname: aHostname [
 
	sshCredentials at: aHostname put: credential
]
