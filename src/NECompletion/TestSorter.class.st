"
Strategy pattern that describes the sorting functionality.

Sorter allSubclasses collect: #kind an OrderedCollection('' 'alphabetical' '' '')
"
Class {
	#name : #TestSorter,
	#superclass : #Object,
	#category : #'NECompletion-New'
}

{ #category : #description }
TestSorter class >> kind [ 
	^ ''
]

{ #category : #sorting }
TestSorter >> sortCompletionList: aList [

	self shouldBeImplemented.
]
