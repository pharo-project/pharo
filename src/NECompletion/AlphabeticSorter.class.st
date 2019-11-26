"
Aplhabetical sorting
"
Class {
	#name : #AlphabeticSorter,
	#superclass : #CompletionSorter,
	#category : #'NECompletion-Sorting'
}

{ #category : #description }
AlphabeticSorter class >> kind [

	^ 'alphabetical'
]

{ #category : #sorting }
AlphabeticSorter >> sortCompletionList: aList [

	^ aList asOrderedCollection sort
]
