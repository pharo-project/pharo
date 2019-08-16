"
One main Entry class instead of NECEntry functionality
"
Class {
	#name : #CompletionEntry,
	#superclass : #Object,
	#instVars : [
		'contents',
		'type',
		'node'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
CompletionEntry class >> contents: aString [ 
	^ self new setContents: aString
]

{ #category : #accessing }
CompletionEntry class >> contents: aString node: aNode [
	^ self new setContents: aString node: aNode
]

{ #category : #accessing }
CompletionEntry >> completion [
	^ self contents asSymbol
]

{ #category : #accessing }
CompletionEntry >> contents [
	^contents
]

{ #category : #initialization }
CompletionEntry >> setContents: aString [ 
	contents := aString.
]

{ #category : #initialization }
CompletionEntry >> setContents: aString node: aNode [
	contents := aString.
	node := aNode
]

{ #category : #initialization }
CompletionEntry >> type [
	"Here we should later return the symbol (see the other implementors of type in the old system
	this will be easy to get form the node"
	^ node isVariable ifTrue:
			[ (Smalltalk globals hasBindingThatBeginsWith: node name) ifTrue: [ ^#globalVar ] ]
]
