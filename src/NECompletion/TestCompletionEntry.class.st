"
One main Entry class instead of NECEntry functionality
"
Class {
	#name : #TestCompletionEntry,
	#superclass : #Object,
	#instVars : [
		'contents',
		'type'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
TestCompletionEntry class >> contents: aString [ 
	^ self new setContents: aString
]

{ #category : #accessing }
TestCompletionEntry >> completion [
	^ self contents asSymbol
]

{ #category : #accessing }
TestCompletionEntry >> contents [
	^contents
]

{ #category : #initialization }
TestCompletionEntry >> setContents: aString [ 
	contents := aString.
]

{ #category : #initialization }
TestCompletionEntry >> type [
	"Here we should later return the symbol (see the other implementors of type in the old system
	this will be easy to get form the node"
	^ type
]
