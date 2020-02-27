"
An attribute to indent text at the left.

------exemple------
| stream |
stream := TextStream on: (Text new: 100).
stream 
	withAttribute: (TextIndent tabs: 1) do: [ stream nextPutAll: 'Pharo is cool'];
	cr;
	withAttribute: (TextIndent tabs: 2) do: [stream nextPutAll: 'Smalltalk is cool'].
TextMorph new 
	newContents: stream contents;
	openInWindow.
"
Class {
	#name : #TextIndent,
	#superclass : #TextAttribute,
	#instVars : [
		'amount'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #'instance creation' }
TextIndent class >> amount: amount [
	"create a TextIndent which will indent by the given amount.  Currently this is a number of tabs, but may change in the futur"
	^super new amount: amount
]

{ #category : #'instance creation' }
TextIndent class >> tabs: numTabs [
	"create an indentation by the given number of tabs"
	^self amount: numTabs
]

{ #category : #access }
TextIndent >> amount [
	"number of tab spaces to indent by"
	^amount
]

{ #category : #access }
TextIndent >> amount: anInteger [
	"change the number of tabs to indent by"
	amount := anInteger
]

{ #category : #condensing }
TextIndent >> dominates: anAttribute [
	^(self class == anAttribute class)
]

{ #category : #printing }
TextIndent >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' amount: '.
	amount printOn: aStream
]
