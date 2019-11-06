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
	CompletionModel sorterClass: AlphabeticSorter
]

{ #category : #sorting }
CompletionSorter >> sortCompletionList: aList [

	self shouldBeImplemented
]
