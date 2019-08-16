"
Strategy pattern that describes the sorting functionality.

Sorter allSubclasses collect: #kind an OrderedCollection('' 'alphabetical' '' '')
"
Class {
	#name : #CompletionSorter,
	#superclass : #Object,
	#category : #'NECompletion-New'
}

{ #category : #description }
CompletionSorter class >> kind [ 
	^ ''
]

{ #category : #'tools registry' }
CompletionSorter class >> register [
	CompletionModel sorter: AlphabeticSorter
]

{ #category : #description }
CompletionSorter class >> sortCompletionList: aList [

	self shouldBeImplemented.
]
