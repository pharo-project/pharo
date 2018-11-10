"
I am a credential provider that requires the missing plaintext credential to the user.
"
Class {
	#name : #IceTipInteractiveCredentialsProvider,
	#superclass : #IceCredentialsProvider,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #private }
IceTipInteractiveCredentialsProvider class >> initialize [
	
	self providerType: nil
]

{ #category : #private }
IceTipInteractiveCredentialsProvider class >> isDefault [
	^ true
]

{ #category : #private }
IceTipInteractiveCredentialsProvider >> obtainPlaintextCredentials: credentialsToReturn [

	| returnedCred |
	returnedCred := IceTipAskForPlaintextCredentialsModel new
		credentialStore: self credentialStore;
		remoteHostname: remote host;
		open.
		
	
	credentialsToReturn readFrom: returnedCred
]

{ #category : #private }
IceTipInteractiveCredentialsProvider >> obtainSshCredentials: credentialsToReturn [ 
	
	| returnedCred |
	
	returnedCred := IceTipAskSSHCredentialsModel new
		credentials: self sshCredentials;
		credentialStore: self credentialStore;
		open.
		
	credentialsToReturn readFrom: returnedCred
]
