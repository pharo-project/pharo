Class {
	#name : #RBEOFTokenTest,
	#superclass : #TestCase,
	#instVars : [
		'token'
	],
	#category : #'AST-Core-Tests-Tokens'
}

{ #category : #running }
RBEOFTokenTest >> setUp [
	super setUp.
	token := RBEOFToken start: (1 to: 10)
]

{ #category : #test }
RBEOFTokenTest >> testLength [

	self assert: token length equals: 0
]

{ #category : #test }
RBEOFTokenTest >> testSource [

	self assert: token source equals: String empty
]

{ #category : #test }
RBEOFTokenTest >> testValue [

	self assert: token value equals: String empty
]

{ #category : #test }
RBEOFTokenTest >> testisEOF [

	self assert: token isEOF
]
