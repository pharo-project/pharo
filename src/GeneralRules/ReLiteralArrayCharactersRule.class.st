"
Literal arrays containing only characters can more efficiently represented as strings.
"
Class {
	#name : #ReLiteralArrayCharactersRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReLiteralArrayCharactersRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayCharactersRule'
]

{ #category : #running }
ReLiteralArrayCharactersRule >> basicCheck: aNode [
	aNode isLiteralArray ifFalse: [ ^ false ].
	aNode value ifEmpty: [ ^ false ].
	^ aNode value allSatisfy: #isCharacter
]

{ #category : #accessing }
ReLiteralArrayCharactersRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReLiteralArrayCharactersRule >> name [
	^ 'Literal array contains only characters'
]
