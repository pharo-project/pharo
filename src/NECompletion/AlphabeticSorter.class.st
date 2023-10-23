"
Aplhabetical sorting
"
Class {
	#name : 'AlphabeticSorter',
	#superclass : 'CompletionSorter',
	#category : 'NECompletion-Sorting',
	#package : 'NECompletion',
	#tag : 'Sorting'
}

{ #category : 'description' }
AlphabeticSorter class >> kind [

	^ 'alphabetical'
]

{ #category : 'sorting' }
AlphabeticSorter >> sortCompletionList: anOrderedCollection [

	^ anOrderedCollection sort
]
