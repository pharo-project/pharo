"
A ProtocolOrganizer is part of a ClassOrganizer. 

It manages the protocols of the class that owns it
"
Class {
	#name : #ProtocolOrganizer,
	#superclass : #Object,
	#instVars : [
		'allProtocol',
		'protocols',
		'organization'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #accessing }
ProtocolOrganizer >> allProtocol [

	^ allProtocol
]

{ #category : #initialization }
ProtocolOrganizer >> initialize [
	super initialize.
	self reset
]

{ #category : #accessing }
ProtocolOrganizer >> organization [

	^ organization
]

{ #category : #accessing }
ProtocolOrganizer >> organization: anObject [

	organization := anObject
]

{ #category : #accessing }
ProtocolOrganizer >> privateProtocols [

	^ protocols
]

{ #category : #accessing }
ProtocolOrganizer >> protocols [

	^ self privateProtocols asArray
]

{ #category : #initialization }
ProtocolOrganizer >> reset [
	protocols := IdentitySet new.
	allProtocol := AllProtocol protocolOrganizer: self
]
