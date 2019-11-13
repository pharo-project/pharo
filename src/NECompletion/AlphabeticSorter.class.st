"
Aplhabetical sorting
"
Class {
	#name : #AlphabeticSorter,
	#superclass : #CompletionSorter,
	#category : #'NECompletion-New'
}

{ #category : #description }
AlphabeticSorter class >> kind [

	^ 'alphabetical'
]

{ #category : #sorting }
AlphabeticSorter >> sortCompletionList: aList [

	^ aList asOrderedCollection sort
]
