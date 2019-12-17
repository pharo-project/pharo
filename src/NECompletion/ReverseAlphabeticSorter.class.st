"
Reverse alphabetic sorting
"
Class {
	#name : #ReverseAlphabeticSorter,
	#superclass : #CompletionSorter,
	#category : #'NECompletion-Sorting'
}

{ #category : #description }
ReverseAlphabeticSorter class >> kind [

	^ 'reverse'
]

{ #category : #sorting }
ReverseAlphabeticSorter >> sortCompletionList: aList [

	^ aList asOrderedCollection sort reverse
]
