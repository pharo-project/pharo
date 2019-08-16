"
Reverse alphabetic sorting
"
Class {
	#name : #ReverseAlphabeticSorter,
	#superclass : #CompletionSorter,
	#category : #'NECompletion-New'
}

{ #category : #description }
ReverseAlphabeticSorter class >> kind [

	^ 'reverse'
]

{ #category : #description }
ReverseAlphabeticSorter class >> sortCompletionList: aList [

	^ aList sorted reverse.
]
