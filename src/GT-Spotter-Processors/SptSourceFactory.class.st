"
I represent the responsibility to create the sources, 
so there could be different strategies to generate the sources, for example with or without indexes.
"
Class {
	#name : #SptSourceFactory,
	#superclass : #Object,
	#classVars : [
		'Current'
	],
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #accessing }
SptSourceFactory class >> current [

	^ Current ifNil: [ self defaultSourceFactory ]
]

{ #category : #'detecting implementations' }
SptSourceFactory class >> defaultSourceFactory [

	| sourceFactoryClass |
	sourceFactoryClass := (self subclasses
		sorted: [ :a :b | a priority > b priority ])
		detect: [ :each | each isAvailable ].
		
	^ sourceFactoryClass new
]

{ #category : #accessing }
SptSourceFactory class >> forTest [

	Current := self new
]

{ #category : #'detecting implementations' }
SptSourceFactory class >> isAvailable [ 

	^ self subclassResponsibility
]

{ #category : #'detecting implementations' }
SptSourceFactory class >> priority [ 

	^ self subclassResponsibility 
]

{ #category : #accessing }
SptSourceFactory class >> reset [

	Current := nil
]

{ #category : #'sources - classes' }
SptSourceFactory >> classesBeginsWithSource [

	 ^ self subclassResponsibility 
]

{ #category : #'sources - classes' }
SptSourceFactory >> classesSubstringSource [

	 ^ self subclassResponsibility 
]

{ #category : #'sources - implementors' }
SptSourceFactory >> implementorsBeginsWithSource [

	^ self subclassResponsibility
]

{ #category : #'sources - implementors' }
SptSourceFactory >> implementorsSubstringSource [

	^ self subclassResponsibility
]
