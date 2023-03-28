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
ProtocolOrganizer >> addProtocolNamed: aName [

	^ self addProtocol: (Protocol name: aName)
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

	^ { allProtocol }, protocols asArray
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> classify: aSymbol inProtocolNamed: aProtocolName [
	| name protocol |
	name := aProtocolName.
	name = allProtocol name ifTrue: [ name := Protocol unclassified ].

	"maybe here we should check if this method already belong to another protocol"
	(self protocolsOfSelector: aSymbol) do: [ :p | p removeMethodSelector: aSymbol ].
	protocol := self protocolNamed: name ifAbsent: [ self addProtocolNamed: name ].

	protocol addMethodSelector: aSymbol
]

{ #category : #private }
ProtocolOrganizer >> existsProtocolNamed: aProtocolName [

	^self allProtocols anySatisfy: [ :e | e name = aProtocolName ]
]

{ #category : #accessing }
ProtocolOrganizer >> extensionProtocols [
	^ self protocols select: #isExtensionProtocol
]

{ #category : #testing }
ProtocolOrganizer >> hasProtocolNamed: aString [

	^ self allProtocols anySatisfy: [ :each | each name = aString ]
]

{ #category : #testing }
ProtocolOrganizer >> includesSelector: selector [
	^ protocols anySatisfy: [ :each | each includesSelector: selector ]
]

{ #category : #initialization }
ProtocolOrganizer >> initialize [
	super initialize.
	self reset
]

{ #category : #'backward compatibility' }
ProtocolOrganizer >> methodsInProtocolNamed: aName [

	aName = AllProtocol defaultName ifTrue: [ ^ self allMethodSelectors ].

	^ (self protocolNamed: aName ifAbsent: [ ^ #(  ) ]) methodSelectors
]

{ #category : #private }
ProtocolOrganizer >> moveMethodsFrom: fromProtocolNamed to: toProtocolNamed [

	| fromProtocol toProtocol |
	fromProtocol := self protocolNamed: fromProtocolNamed.
	toProtocol := self protocolNamed: toProtocolNamed.

	toProtocol addAllMethodsFrom: fromProtocol.
	fromProtocol removeAllMethodSelectors.

	^ toProtocol
]

{ #category : #accessing }
ProtocolOrganizer >> protocolNamed: aName [

	^ self protocolNamed: aName ifAbsent: [ NotFound signalFor: aName ]
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

{ #category : #protocol }
ProtocolOrganizer >> protocolsOfSelector: aSelector [

	^ (self protocols select: [:each | each includesSelector: aSelector ]) asArray
]

{ #category : #protocol }
ProtocolOrganizer >> protocolsSorted [
	^ (self protocols collect: #name as: Array) sort
		copyWithFirst: allProtocol name
]

{ #category : #'protocol - removing' }
ProtocolOrganizer >> removeEmptyProtocols [
	| removedProtocols |
	removedProtocols := protocols select: [:e | e isEmpty and: [ e canBeRemoved ] ].
	removedProtocols do: [:each | self removeProtocol: each].
	^removedProtocols
]

{ #category : #accessing }
ProtocolOrganizer >> removeMethod: aSymbol [

	(self protocolsOfSelector: aSymbol) do: [ :p | p removeMethodSelector: aSymbol ]
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

{ #category : #accessing }
ProtocolOrganizer >> renameProtocol: oldName into: newName [

	(self hasProtocolNamed: oldName) ifFalse: [ ^ self ].

	(self existsProtocolNamed: newName)
		ifTrue: [
			self moveMethodsFrom: oldName to: newName.
			self removeProtocolNamed: oldName ]
		ifFalse: [
			^ (self protocolNamed: oldName)
				  name: newName;
				  yourself ]
]

{ #category : #initialization }
ProtocolOrganizer >> reset [
	protocols := IdentitySet new.
	allProtocol := AllProtocol protocolOrganizer: self
]
