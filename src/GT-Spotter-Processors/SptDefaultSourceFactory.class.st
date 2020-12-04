"
I represent the creation of sources without indexes. 

"
Class {
	#name : #SptDefaultSourceFactory,
	#superclass : #SptSourceFactory,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'detecting implementations' }
SptDefaultSourceFactory class >> isAvailable [ 

	"I am always available, I am the default implementation"
	^ true
]

{ #category : #'detecting implementations' }
SptDefaultSourceFactory class >> priority [ 
	
	"I have a lower priority, because I don't use any fancy scheme"
	^ 0 
]

{ #category : #'sources - classes' }
SptDefaultSourceFactory >> classesBeginsWithSource [

	^ ((SptClassIterator new select: [ :e | e isMeta not ])
		collect: [ :e | SptClassEntry on: e ]) asBeginsWithFilter
]

{ #category : #'sources - classes' }
SptDefaultSourceFactory >> classesSubstringSource [

	^ ((SptClassIterator new select: [ :e | e isMeta not ])
		collect: [ :e | SptClassEntry on: e ]) asSubstringFilter
]

{ #category : #'sources - implementors' }
SptDefaultSourceFactory >> implementorsBeginsWithSource [

	^ (SptImplementorsIterator new collect: [ :e | SptMethodEntry on: e ]) asBeginsWithFilter
]

{ #category : #'sources - implementors' }
SptDefaultSourceFactory >> implementorsSubstringSource [

	^ (SptImplementorsIterator new collect: [ :e | SptMethodEntry on: e ]) asSubstringFilter
]
