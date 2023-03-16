"
I represent a syntax or a semantic error in some pharo code.

My instances are signaled by the parser or the compiler.
"
Class {
	#name : #CodeError,
	#superclass : #Error,
	#instVars : [
		'node',
		'location',
		'sourceCode'
	],
	#category : #'AST-Core-Exception'
}

{ #category : #accessing }
CodeError >> errorCode [

	self deprecated: 'use errorCode'.

	^ self sourceCode
]

{ #category : #accessing }
CodeError >> errorMessage [

	self deprecated: 'use messageText'.

	^ messageText
]

{ #category : #accessing }
CodeError >> location [

	location ifNotNil: [ ^ location  ].
	node ifNotNil: [ ^ node start ].
	^ nil
]

{ #category : #accessing }
CodeError >> location: anInteger [

	location := anInteger
]

{ #category : #accessing }
CodeError >> methodClass [
	^self methodNode methodClass
]

{ #category : #accessing }
CodeError >> methodNode [
	^node methodNode
]

{ #category : #accessing }
CodeError >> node [
	^ node
]

{ #category : #accessing }
CodeError >> node: aNode [
	node := aNode
]

{ #category : #accessing }
CodeError >> sourceCode [

	sourceCode ifNotNil: [  ^ sourceCode ].
	node ifNotNil: [  ^ node sourceCode ].
	^ nil
]

{ #category : #accessing }
CodeError >> sourceCode: aString [

	sourceCode := aString
]
