"
Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.
"
Class {
	#name : 'RePrecedenceRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
RePrecedenceRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
RePrecedenceRule class >> ruleName [
	^ 'Inspect instances of "A + B * C" might be "A + (B * C)"'
]

{ #category : 'accessing' }
RePrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'PrecedenceRule'
]

{ #category : 'running' }
RePrecedenceRule >> basicCheck: aNode [
	| leftOperand |
	aNode isMessage ifFalse: [ ^ false ].
	aNode selector = #* ifFalse: [ ^ false ].
	leftOperand := aNode receiver.
	leftOperand isMessage ifFalse: [ ^ false ].
	leftOperand hasParentheses ifTrue: [ ^ false ].
	^ #(#+ #-) includes: leftOperand selector
]
