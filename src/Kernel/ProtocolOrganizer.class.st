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

{ #category : #import }
ProtocolOrganizer class >> fromSpec: aSpec [
	
	^ self new
		fromSpec: aSpec;
		yourself
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> addProtocol: aProtocol [

	^ protocols add: aProtocol
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> addProtocolNamed: aName [
	^ protocols add: (Protocol name: aName)
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

{ #category : #accessing }
ProtocolOrganizer >> allProtocolsNames [

	^ self allProtocols collect: #name
]

{ #category : #'protocol - adding' }
ProtocolOrganizer >> classify: aSymbol inProtocolNamed: aProtocolName [
	| name protocol |
	name := aProtocolName.
	name = allProtocol name ifTrue: [ name := Protocol unclassified ].

	"maybe here we should check if this method already belong to another protocol"
	(self protocolsOfSelector: aSymbol) do: [ :p | p removeMethodSelector: aSymbol ].
	protocol := self getProtocolNamed: name ifNone: [ self addProtocolNamed: name ].

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

{ #category : #importing }
ProtocolOrganizer >> fromSpec: aSpec [
	aSpec
		do: [ :spec | 
			| name methods |
			name := spec first asSymbol.
			methods := spec allButFirst asSet.
			self addProtocol: (Protocol name: name methodSelectors: methods) ]
]

{ #category : #protocol }
ProtocolOrganizer >> getProtocolNamed: aByteString ifNone: aBlockClosure [ 
	
	^ protocols detect: [:e | e name = aByteString ] ifNone: aBlockClosure 
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

	^ (self protocolNamed: aName) methodSelectors
]

{ #category : #private }
ProtocolOrganizer >> moveMethodsFrom: fromProtocolNamed to: toProtocolNamed [
	| fromProtocol toProtocol |
	
	fromProtocol := self protocolNamed: fromProtocolNamed.
	toProtocol := self protocolNamed: toProtocolNamed.
	
	toProtocol addAllMethodsFrom: fromProtocol.
	fromProtocol removeAllMethodSelectors.
	
	^ toProtocol.
]

{ #category : #accessing }
ProtocolOrganizer >> protocolNamed: aName [

	^ self 
		protocolNamed: aName 
		ifAbsent: [ Protocol empty ]
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

{ #category : #accessing }
ProtocolOrganizer >> protocolsNames [

	^ protocols 
		collect: #name 
		as: Array
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
	
	aProtocol canBeRemoved
		ifFalse: [ 
			"Virtual protocols who cannot be removed should not raise an error"
			aProtocol isVirtualProtocol 
				ifTrue: [ ^ self ].
			ProtocolRemovalException signal ].
	
	^ protocols 
		remove: aProtocol
		ifAbsent: [ ]
]

{ #category : #'protocol - removing' }
ProtocolOrganizer >> removeProtocolNamed: aName [
	| protocolToRemove |
	
	protocolToRemove := self protocolNamed: aName.
	^ self removeProtocol: protocolToRemove
]

{ #category : #accessing }
ProtocolOrganizer >> renameProtocol: oldName into: newName [
	(self existsProtocolNamed: newName) 
		ifTrue: [ 
			self moveMethodsFrom: oldName to: newName.
			self removeProtocolNamed: oldName ] 
		ifFalse: [ 
			^ (self protocolNamed: oldName)
				name: newName;
				yourself ].
]

{ #category : #initialization }
ProtocolOrganizer >> reset [
	protocols := IdentitySet new.
	allProtocol := AllProtocol protocolOrganizer: self.
]
