"
I represent the no-reference. I implement the null object design pattern.
"
Class {
	#name : #OmNullReference,
	#superclass : #OmAbstractReference,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Ombu-Entries'
}

{ #category : #'ston persistence' }
OmNullReference class >> fromSton: stonReader [

	stonReader parseListDo: [ :each :index | "nothing to read" ].

	^ self uniqueInstance
]

{ #category : #'instance creation' }
OmNullReference class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #testing }
OmNullReference >> isNull [
	^ true
]

{ #category : #printing }
OmNullReference >> printOn: aStream [
	aStream 
		nextPutAll: 'null:reference'
]

{ #category : #'ston persistence' }
OmNullReference >> stonContainSubObjects [ 
	^ false
]

{ #category : #'ston persistence' }
OmNullReference >> stonOn: stonWriter [

	stonWriter 
		writeObject: self 
		streamShortList: [ :writer |  ]
]
