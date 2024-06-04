"
Literal arrays containing only characters can more efficiently represented as strings.
"
Class {
	#name : 'ReLiteralArrayCharactersRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReLiteralArrayCharactersRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReLiteralArrayCharactersRule class >> ruleName [
	^ 'Literal array contains only characters'
]

{ #category : 'accessing' }
ReLiteralArrayCharactersRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'LiteralArrayCharactersRule'
]

{ #category : 'running' }
ReLiteralArrayCharactersRule >> basicCheck: aNode [
	aNode isLiteralArray ifFalse: [ ^ false ].
	aNode value ifEmpty: [ ^ false ].
	^ aNode value allSatisfy: #isCharacter
]
