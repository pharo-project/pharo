"
Checks for mathematical expressions that might be evaluated different (from left-to-right) than the developer thinks.
"
Class {
	#name : #RBPrecedenceRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBPrecedenceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'PrecedenceRule'
]

{ #category : #running }
RBPrecedenceRule >> check: aNode forCritiquesDo: aCriticBlock [
	| leftOperand |
	
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #* ifFalse: [ ^ self ].
	leftOperand := aNode receiver.
	leftOperand isMessage ifFalse: [ ^ self ].
	leftOperand hasParentheses ifTrue: [ ^ self ].
	(#(#+ #-) includes: leftOperand selector) ifFalse: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aNode)
]

{ #category : #accessing }
RBPrecedenceRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBPrecedenceRule >> name [
	^ 'Inspect instances of "A + B * C" might be "A + (B * C)"'
]
