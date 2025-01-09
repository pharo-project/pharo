"
I'm a class that does sorting based on the size of the context itself.

I return a list sorted by size.


"
Class {
	#name : 'SizeSorter',
	#superclass : 'CompletionSorter',
	#category : 'NECompletion-Sorting',
	#package : 'NECompletion',
	#tag : 'Sorting'
}

{ #category : 'sorting' }
SizeSorter >> sortCompletionList: anOrderedCollection [

	^ anOrderedCollection sort: [ :a :b | a contents size < b contents size ]
]
