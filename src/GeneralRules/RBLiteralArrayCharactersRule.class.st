"
Literal arrays containing only characters can more efficiently represented as strings.
"
Class {
	#name : #RBLiteralArrayCharactersRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBLiteralArrayCharactersRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayCharactersRule'
]

{ #category : #running }
RBLiteralArrayCharactersRule >> basicCheck: aNode [
	aNode isLiteralArray ifFalse: [ ^ false ].
	aNode value ifEmpty: [ ^ false ].
	^ aNode value allSatisfy: #isCharacter
]

{ #category : #accessing }
RBLiteralArrayCharactersRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBLiteralArrayCharactersRule >> name [
	^ 'Literal array contains only characters'
]
