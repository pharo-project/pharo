"
I act as a web browser history
"
Class {
	#name : #NavigationHistory,
	#superclass : #Object,
	#instVars : [
		'storage',
		'index',
		'maxSize',
		'paused'
	],
	#category : #'Tool-Base-Utilities'
}

{ #category : #accessing }
NavigationHistory class >> defaultMaxSize [

	^ 15
]

{ #category : #'collection compatibility' }
NavigationHistory >> add: anEntry [

	self addEntry: anEntry
]

{ #category : #protocol }
NavigationHistory >> addEntry: anEntry [
	
	self isPaused ifTrue: [ ^ self ].
	
	self basicAdd: anEntry
]

{ #category : #protocol }
NavigationHistory >> back [

	index := ((index - 1) max: 0)
]

{ #category : #protocol }
NavigationHistory >> basicAdd: anEntry [
	
	( storage isNotEmpty and: [ storage last = anEntry ] ) ifTrue: [ ^ self ].
	anEntry = self current key ifTrue: [ ^ self ].
	self checkSize.
	index := index +1.
	index > storage size
		ifTrue: [ storage addLast:  (anEntry -> DateAndTime now)]
		ifFalse: [ 
			storage := storage copyFrom: 1 to: index.
			storage at: index put: (anEntry -> DateAndTime now)]
]

{ #category : #private }
NavigationHistory >> checkSize [
	
	[ storage size >= self maxSize ]
		whileTrue: [ 
			storage removeFirst.
			index := index -1].
]

{ #category : #protocol }
NavigationHistory >> current [

	^ storage at: index ifAbsent: [ nil -> nil ]
]

{ #category : #protocol }
NavigationHistory >> entries [

	^ storage
]

{ #category : #testing }
NavigationHistory >> hasNext [

	^ index < storage size
]

{ #category : #testing }
NavigationHistory >> hasPrevious [

	^ index > 1
]

{ #category : #initialization }
NavigationHistory >> initialize [

	storage := OrderedCollection new.
	index := 0.
	paused := false.
]

{ #category : #testing }
NavigationHistory >> isPaused [
	^ paused
]

{ #category : #protocol }
NavigationHistory >> last [

	^ storage last
]

{ #category : #protocol }
NavigationHistory >> maxSize [

	^ maxSize ifNil: [ maxSize := self class defaultMaxSize ].
]

{ #category : #protocol }
NavigationHistory >> maxSize: anInteger [

	maxSize := anInteger
]

{ #category : #protocol }
NavigationHistory >> next [

	index := ((index + 1) min: storage size).
	^ storage at: index
]

{ #category : #protocol }
NavigationHistory >> pauseDuring: aBlock [
	| previousPausedValue |
	"Don't accept any new entries while executing aBlock"
	previousPausedValue := paused.
	paused := true.
	aBlock ensure: [ paused := previousPausedValue ]
]

{ #category : #protocol }
NavigationHistory >> previous [

	index := index-1 max: 1.
	^ storage at: index
]

{ #category : #protocol }
NavigationHistory >> removeEntry: anEntry [
	| toRemove |
	
	"we know that toRemove will always contains one element"
	toRemove := (storage select: [:e | e key == anEntry ]).
	toRemove ifEmpty: [ ^ self ].
	toRemove := toRemove first.
	storage remove: toRemove.
	storage addLast: toRemove
]

{ #category : #protocol }
NavigationHistory >> removeLastEntry [

	self isPaused ifTrue: [ ^ self ].
	storage ifEmpty: [ ^ self ].
	
	storage remove: storage last.	
	index := index -1
]

{ #category : #protocol }
NavigationHistory >> replaceCurrentWith: anEntry [
	"Change the current history item to the given object"
	storage at: index put: (anEntry -> DateAndTime now)
]
