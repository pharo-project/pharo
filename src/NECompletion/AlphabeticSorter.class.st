"
Aplhabetical sorting
"
Class {
	#name : 'AlphabeticSorter',
	#superclass : 'CoCompletionSorter',
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
