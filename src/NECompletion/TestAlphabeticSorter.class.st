"
Aplhabetical sorting
"
Class {
	#name : #TestAlphabeticSorter,
	#superclass : #TestSorter,
	#category : #'NECompletion-New'
}

{ #category : #description }
TestAlphabeticSorter class >> kind [

	^ 'alphabetical'
]

{ #category : #description }
TestAlphabeticSorter class >> sortCompletionList: aList [

	^ aList sorted.
]
