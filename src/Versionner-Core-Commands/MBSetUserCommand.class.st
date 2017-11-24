"
Command to bset the user in Versionner
"
Class {
	#name : #MBSetUserCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #'as yet unclassified' }
MBSetUserCommand >> documentation [
	^ 'Set the user and password used when saving and loading packages and configurations.'
]

{ #category : #'as yet unclassified' }
MBSetUserCommand >> execute [
	target configurationRoot username: nil.
	target configurationRoot password: nil.
	self username.
	self password.
]

{ #category : #'as yet unclassified' }
MBSetUserCommand >> title [
	^ 'set user and password'
]
