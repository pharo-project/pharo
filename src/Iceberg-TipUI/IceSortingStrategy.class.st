"
I am a sorting strategy taking as parameter a collection and returning the collection sorted. The user can customize the sorting to get, for example, the modified entities first or last. 


Example


	IceSortingStrategy new first: #modified; last: #notLoaded; sort: aCollection

 
Internal Representation and Key Implementation Points.

    Instance Variables
	first:		<aSymbol>		Represent the entities to display first.
	last:		<aSymbol>		Represent the entities to display last.
"
Class {
	#name : #IceSortingStrategy,
	#superclass : #Object,
	#instVars : [
		'first',
		'last'
	],
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceSortingStrategy class >> possibleValues [
	^ #(#none #modified #notLoaded)
]

{ #category : #accessing }
IceSortingStrategy >> first [
	^ first
]

{ #category : #accessing }
IceSortingStrategy >> first: anObject [
	first := anObject.
	
	(anObject ~= #none and: [ self last = anObject ])
		ifTrue: [ self last: #none ]
]

{ #category : #initialization }
IceSortingStrategy >> initialize [
	super initialize.
	first := #modified.
	last := #notLoaded
]

{ #category : #accessing }
IceSortingStrategy >> last [
	^ last
]

{ #category : #accessing }
IceSortingStrategy >> last: anObject [
	last := anObject.
	
	(anObject ~= #none and: [ self first = anObject ])
		ifTrue: [ self first: #none ]
]

{ #category : #accessing }
IceSortingStrategy >> selectMatching: aSymbol from: aCollection [
	aSymbol = #modified
		ifTrue: [ ^ aCollection select: #isModified ].
	aSymbol = #notLoaded
		ifTrue: [ ^ aCollection reject: #isLoaded ].
	self error: 'This sorting is not accepted: ' , aSymbol
]

{ #category : #sorting }
IceSortingStrategy >> sort: aListOfPackages [
	| result packages lasts |
	packages := aListOfPackages.
	result := OrderedCollection new.
	self first = #none ifFalse: [ result addAll: (self sortByName: (self selectMatching: self first from: packages)) ].
	packages := packages copyWithoutAll: result.
	lasts := self last = #none ifTrue: [ {} ] ifFalse: [ self selectMatching: self last from: packages ].
	packages := packages copyWithoutAll: lasts.
	result addAll: (self sortByName: packages).
	result addAll: (self sortByName: lasts).
	^ result
]

{ #category : #sorting }
IceSortingStrategy >> sortByName: aCollection [
	^ aCollection sorted: #packageName ascending
]

{ #category : #accessing }
IceSortingStrategy >> withOthers: aSymbol [ 
	self first = aSymbol ifTrue: [ self first: nil ].
	self last = aSymbol ifTrue: [ self last: nil ]
]
