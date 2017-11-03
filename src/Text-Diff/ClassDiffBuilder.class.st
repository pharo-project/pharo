"
I'm like TextDiffBuilder, but I split the input text by Character >> #separators, instead of new lines. 
I'm probably ment to create diffs of class definitions.
"
Class {
	#name : #ClassDiffBuilder,
	#superclass : #TextDiffBuilder,
	#category : #Text-Diff
}

{ #category : #private }
ClassDiffBuilder >> print: aString withAttributes: attributes on: stream [

	stream
		withAttributes: attributes 
		do: [ stream nextPutAll: aString ]
]

{ #category : #private }
ClassDiffBuilder >> split: aString [
	"I return an array with aString splitted by Character >> #separators."

	^Array streamContents: [ :stream |
		| out |
		out := aString copy writeStream.
		aString do: [ :c |
			out nextPut: c.
			c isSeparator ifTrue:[
				stream nextPut: out contents.
				out reset ] ].
		out position = 0 ifFalse: [ 
			stream nextPut: out contents ] ]
]
