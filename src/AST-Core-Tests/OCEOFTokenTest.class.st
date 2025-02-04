Class {
	#name : 'OCEOFTokenTest',
	#superclass : 'TestCase',
	#instVars : [
		'token'
	],
	#category : 'AST-Core-Tests-Tokens',
	#package : 'AST-Core-Tests',
	#tag : 'Tokens'
}

{ #category : 'running' }
OCEOFTokenTest >> setUp [
	super setUp.
	token := OCEOFToken start: (1 to: 10)
]

{ #category : 'tests' }
OCEOFTokenTest >> testLength [

	self assert: token length equals: 0
]

{ #category : 'tests' }
OCEOFTokenTest >> testSource [

	self assert: token source equals: String empty
]

{ #category : 'tests' }
OCEOFTokenTest >> testValue [

	self assert: token value equals: String empty
]

{ #category : 'tests' }
OCEOFTokenTest >> testisEOF [

	self assert: token isEOF
]
