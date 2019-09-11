"
I represent a reference to an entry into a store.
"
Class {
	#name : #OmReference,
	#superclass : #OmAbstractReference,
	#instVars : [
		'localName',
		'globalName'
	],
	#category : #'Ombu-Entries'
}

{ #category : #'ston persistence' }
OmReference class >> fromSton: stonReader [
	| array |
	array := Array new: 2 streamContents: [ :stream |
		stonReader parseListDo: [ :each :index | stream nextPut: each ] ].

	"Optimization: only write globalName if it belongs to a 'remote' store"
	^ array size = 1
		ifTrue: [ 
			self 
				globalName: OmCurrentGlobalName value
				localName: array first ]
		ifFalse: [
			self 
				globalName: array first
				localName: array second ]

]

{ #category : #'instance creation' }
OmReference class >> globalName: aName localName: anIdentifier [

	^ self new
		initializeWithLocalName: anIdentifier
		globalName: aName;
		yourself
]

{ #category : #comparing }
OmReference >> = other [
	^ other species = self species and: [ 
		other localName = self localName and: [
		other globalName = self globalName ] ]
]

{ #category : #accessing }
OmReference >> globalName [
	
	^ globalName
]

{ #category : #comparing }
OmReference >> hash [
	^ (self species hash bitXor: self localName hash) bitXor: self globalName hash
]

{ #category : #initialization }
OmReference >> initializeWithLocalName: aLocalName globalName: aGlobalName [

	self initialize.
	localName := aLocalName.
	globalName := aGlobalName.
]

{ #category : #accessing }
OmReference >> localName [
	
	^ localName
]

{ #category : #printing }
OmReference >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: self shortName; 
		nextPut: $).
]

{ #category : #printing }
OmReference >> shortName [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: self globalName; 
			nextPut: $:;
			nextPutAll: self localName ]
]

{ #category : #'ston persistence' }
OmReference >> stonContainSubObjects [ 
	^ false
]

{ #category : #'ston persistence' }
OmReference >> stonOn: stonWriter [
	
	stonWriter 
		writeObject: self 
		streamShortList: [ :array |
			"Optimization: only write globalName if it belongs to a 'remote' store"
			OmCurrentGlobalName value = self globalName
				ifFalse: [ array add: self globalName ].

			array add: self localName ]
]
