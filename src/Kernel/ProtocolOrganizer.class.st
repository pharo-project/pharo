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

{ #category : #'protocol - adding' }
ProtocolOrganizer >> addProtocol: aProtocol [

	^ protocols add: aProtocol
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> addProtocolNamed: protocolName [

	^ self addProtocol: (Protocol name: protocolName).
]

{ #category : #accessing }
ProtocolOrganizer >> allMethodSelectors [

	^ self organization allMethodSelectors
]

{ #category : #accessing }
ProtocolOrganizer >> allProtocol [

	^ allProtocol
]

{ #category : #accessing }
ProtocolOrganizer >> allProtocols [

	^ self organization allProtocols
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> classify: aSymbol inProtocolNamed: aProtocolName [

	| name protocol |
	name := aProtocolName.
	name = allProtocol name ifTrue: [ name := Protocol unclassified ].

	"maybe here we should check if this method already belong to another protocol"
	self protocols
		select: [ :p | p includesSelector: aSymbol ]
		thenDo: [ :p | p removeMethodSelector: aSymbol ].
	protocol := self protocolNamed: name ifAbsent: [ self addProtocolNamed: name ].

	protocol addMethodSelector: aSymbol
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
ProtocolOrganizer >> protocolNamed: aString ifAbsent: aBlock [

	^ self organization protocolNamed: aString ifAbsent: aBlock
]

{ #category : #accessing }
ProtocolOrganizer >> protocols [

	^ protocols asArray
]

{ #category : #'protocol - removing' }
ProtocolOrganizer >> removeEmptyProtocols [
	| removedProtocols |
	removedProtocols := protocols select: [:e | e isEmpty and: [ e canBeRemoved ] ].
	removedProtocols do: [:each | self removeProtocol: each].
	^removedProtocols
]

{ #category : #'protocol - removing' }
ProtocolOrganizer >> removeProtocol: aProtocol [

	aProtocol canBeRemoved ifFalse:  [ ^ self ].
	^ protocols
		remove: aProtocol
		ifAbsent: [ ]
]

{ #category : #initialization }
ProtocolOrganizer >> reset [
	protocols := IdentitySet new.
	allProtocol := AllProtocol protocolOrganizer: self
]
