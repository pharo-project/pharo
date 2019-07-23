"
Aplhabetical sorting.
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

{ #category : #sorting }
TestAlphabeticSorter >> sortCompletionList: aList [

	^ aList sorted.
]
