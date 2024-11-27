Class {
	#name : 'ASTEOFTokenTest',
	#superclass : 'TestCase',
	#instVars : [
		'token'
	],
	#category : 'AST-Core-Tests-Tokens',
	#package : 'AST-Core-Tests',
	#tag : 'Tokens'
}

{ #category : 'running' }
ASTEOFTokenTest >> setUp [
	super setUp.
	token := ASTEOFToken start: (1 to: 10)
]

{ #category : 'tests' }
ASTEOFTokenTest >> testLength [

	self assert: token length equals: 0
]

{ #category : 'tests' }
ASTEOFTokenTest >> testSource [

	self assert: token source equals: String empty
]

{ #category : 'tests' }
ASTEOFTokenTest >> testValue [

	self assert: token value equals: String empty
]

{ #category : 'tests' }
ASTEOFTokenTest >> testisEOF [

	self assert: token isEOF
]
