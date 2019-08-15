"
One main Entry class instead of NECEntry functionality
"
Class {
	#name : #TestCompletionEntry,
	#superclass : #Object,
	#instVars : [
		'contents',
		'type',
		'node'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
TestCompletionEntry class >> contents: aString [ 
	^ self new setContents: aString
]

{ #category : #accessing }
TestCompletionEntry class >> contents: aString node: aNode [
	^ self new setContents: aString node: aNode
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
TestCompletionEntry >> setContents: aString node: aNode [
	contents := aString.
	node := aNode
]

{ #category : #initialization }
TestCompletionEntry >> type [
	"Here we should later return the symbol (see the other implementors of type in the old system
	this will be easy to get form the node"
	^ node isVariable ifTrue:
			[ (Smalltalk globals hasBindingThatBeginsWith: node name) ifTrue: [ ^#globalVar ] ]
]
