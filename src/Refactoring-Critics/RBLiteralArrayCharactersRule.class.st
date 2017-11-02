"
Literal arrays containing only characters can more efficiently represented as strings.
"
Class {
	#name : #RBLiteralArrayCharactersRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBLiteralArrayCharactersRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayCharactersRule'
]

{ #category : #accessing }
RBLiteralArrayCharactersRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBLiteralArrayCharactersRule >> initialize [
	super initialize.
	self matcher 
		matches: '`#literal'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(node value class == Array and: [ self isArrayOfCharacters: node value ]) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #private }
RBLiteralArrayCharactersRule >> isArrayOfCharacters: anArray [ 
	anArray isEmpty ifTrue: [^false].
	1 to: anArray size
		do: [:each | (anArray at: each) class == Character ifFalse: [^false]].
	^true
]

{ #category : #accessing }
RBLiteralArrayCharactersRule >> name [
	^ 'Literal array contains only characters'
]
