"
I am a parser for generating an AST of classes from a string, i am only used in tests for the moment
"
Class {
	#name : #CodeParser,
	#superclass : #Object,
	#instVars : [
		'originalString'
	],
	#category : #'ClassParser-Model'
}

{ #category : #parsing }
CodeParser class >> parse: aString [ 
	
	^ self new parse: aString
]

{ #category : #parsing }
CodeParser >> parse: aString [
	"Return an AST"
	 
	| ast |
	^ [ RBParser parseMethod: aString ]
		on: Notification
		do: [ ast := RBParser parseExpression: aString.
				[ CDClassDefinitionParser fromASTNode: ast ]
					on: Notification
					do: [ ast ] ]
]
