"
I represent error and warning information on a AST node.
"
Class {
	#name : #RBNotice,
	#superclass : #Object,
	#instVars : [
		'messageText',
		'node'
	],
	#category : #'AST-Core-Notice'
}

{ #category : #testing }
RBNotice >> isError [

	^ false
]

{ #category : #testing }
RBNotice >> isWarning [

	^ false
]

{ #category : #accessing }
RBNotice >> messageText [

	^ messageText
]

{ #category : #signaling }
RBNotice >> messageText: aString [

	messageText := aString
]

{ #category : #accessing }
RBNotice >> node [

	^ node
]

{ #category : #accessing }
RBNotice >> node: anObject [

	node := anObject
]

{ #category : #'error handling' }
RBNotice >> position [

	^ node start
]
