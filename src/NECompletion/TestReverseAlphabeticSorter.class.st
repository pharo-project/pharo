"
Reverse alphabetic sorting
"
Class {
	#name : #TestReverseAlphabeticSorter,
	#superclass : #TestSorter,
	#category : #'NECompletion-New'
}

{ #category : #description }
TestReverseAlphabeticSorter class >> kind [

	^ 'reverse'
]

{ #category : #description }
TestReverseAlphabeticSorter class >> sortCompletionList: aList [

	^ aList sorted reverse.
]
