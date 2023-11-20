Class {
	#name : 'RBEOFTokenTest',
	#superclass : 'TestCase',
	#instVars : [
		'token'
	],
	#category : 'AST-Core-Tests-Tokens',
	#package : 'AST-Core-Tests',
	#tag : 'Tokens'
}

{ #category : 'running' }
RBEOFTokenTest >> setUp [
	super setUp.
	token := RBEOFToken start: (1 to: 10)
]

{ #category : 'tests' }
RBEOFTokenTest >> testLength [

	self assert: token length equals: 0
]

{ #category : 'tests' }
RBEOFTokenTest >> testSource [

	self assert: token source equals: String empty
]

{ #category : 'tests' }
RBEOFTokenTest >> testValue [

	self assert: token value equals: String empty
]

{ #category : 'tests' }
RBEOFTokenTest >> testisEOF [

	self assert: token isEOF
]
