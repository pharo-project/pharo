"
A ProtocolOrganizer is part of a ClassOrganizer. 

It manages the protocols of the class that owns it
"
Class {
	#name : #ProtocolOrganizer,
	#superclass : #Object,
	#instVars : [
		'allProtocol',
		'protocols'
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

	^ self protocols flatCollect: [ :p | p methodSelectors ]
]

{ #category : #accessing }
ProtocolOrganizer >> allProtocol [

	^ allProtocol
]

{ #category : #accessing }
ProtocolOrganizer >> allProtocols [

	^ { self allProtocol } , self protocols
]

{ #category : #testing }
ProtocolOrganizer >> hasProtocolNamed: aString [

	^ self allProtocols anySatisfy: [ :each | each name = aString ]
]

{ #category : #initialization }
ProtocolOrganizer >> initialize [
	super initialize.
	self reset
]

{ #category : #accessing }
ProtocolOrganizer >> protocolNamed: aString ifAbsent: aBlock [
	^  self allProtocols
		detect: [ :e | e name = aString ]
		ifNone: aBlock
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

{ #category : #'protocol - removing' }
ProtocolOrganizer >> removeProtocolNamed: aName [

	^ self removeProtocol: (self protocolNamed: aName ifAbsent: [ ^ self ])
]

{ #category : #initialization }
ProtocolOrganizer >> reset [
	protocols := IdentitySet new.
	allProtocol := AllProtocol protocolOrganizer: self
]
