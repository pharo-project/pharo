Class {
	#name : #IceTipAskSSHCredentialsModel,
	#superclass : #IceTipEditSSHCredentialsModel,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #actions }
IceTipAskSSHCredentialsModel >> cancelAction [
	accepted := true.
	self window close.
	LGitNoCredentialsProvided signal
]
