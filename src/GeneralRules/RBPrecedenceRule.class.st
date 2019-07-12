"
Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.
"
Class {
	#name : #RBPrecedenceRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBPrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'PrecedenceRule'
]

{ #category : #running }
RBPrecedenceRule >> basicCheck: aNode [
	| leftOperand |
	aNode isMessage ifFalse: [ ^ false ].
	aNode selector = #* ifFalse: [ ^ false ].
	leftOperand := aNode receiver.
	leftOperand isMessage ifFalse: [ ^ false ].
	leftOperand hasParentheses ifTrue: [ ^ false ].
	^ #(#+ #-) includes: leftOperand selector
]

{ #category : #accessing }
RBPrecedenceRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBPrecedenceRule >> name [
	^ 'Inspect instances of "A + B * C" might be "A + (B * C)"'
]
