"
I wrap a block and expose it as an iterator
"
Class {
	#name : #SptGeneratorBlockIterator,
	#superclass : #SptGenericGenerator,
	#instVars : [
		'block'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptGeneratorBlockIterator class >> on: aBlock [

	^ self basicNew
		block: aBlock;
		initialize;
		yourself
]

{ #category : #accessing }
SptGeneratorBlockIterator >> block [
	^ block
]

{ #category : #accessing }
SptGeneratorBlockIterator >> block: anObject [
	block := anObject
]

{ #category : #enumerating }
SptGeneratorBlockIterator >> elementsDo: aValuable [

	block value: aValuable
]
