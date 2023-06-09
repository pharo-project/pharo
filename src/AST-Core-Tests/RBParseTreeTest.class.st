Class {
	#name : #RBParseTreeTest,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #testing }
RBParseTreeTest class >> isAbstract [

	^self name = #RBParseTreeTest
]

{ #category : #helpers }
RBParseTreeTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : #helpers }
RBParseTreeTest >> parseFaultyExpression: aString [
	^ self parserClass parseFaultyExpression: aString
]

{ #category : #private }
RBParseTreeTest >> parseFaultyMethod: text [
	^ self parserClass parseFaultyMethod: text
]

{ #category : #helpers }
RBParseTreeTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #helpers }
RBParseTreeTest >> parseRewriteMethod: aString [
	^ self parserClass parseRewriteMethod: aString
]

{ #category : #helpers }
RBParseTreeTest >> parserClass [
	^ RBParser
]
