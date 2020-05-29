"
I wrap an existing collection and expose it as a iterator
"
Class {
	#name : #SptCollectionIterator,
	#superclass : #SptGenericGenerator,
	#instVars : [
		'collection'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptCollectionIterator class >> on: aSequenceableCollection [

	^ self basicNew
		collection: aSequenceableCollection;
		initialize;
		yourself
]

{ #category : #accessing }
SptCollectionIterator >> collection [
	^ collection
]

{ #category : #accessing }
SptCollectionIterator >> collection: anObject [
	collection := anObject
]

{ #category : #enumerating }
SptCollectionIterator >> elementsDo: aValuable [

	collection do: aValuable 
]
