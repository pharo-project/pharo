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

{ #category : #description }
AlphabeticSorter class >> sortCompletionList: aList [

	^ aList sorted.
]
