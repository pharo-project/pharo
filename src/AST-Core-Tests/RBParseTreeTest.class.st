Class {
	#name : #RBParseTreeTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #testing }
RBParseTreeTest class >> isAbstract [ 

	^self name = #RBParseTreeTest
]

{ #category : #helpers }
RBParseTreeTest >> parseExpression: aString [ 

	^ RBParser parseExpression: aString
]

{ #category : #helpers }
RBParseTreeTest >> parseFaultyExpression: aString [ 

	^ RBParser parseFaultyExpression: aString
]

{ #category : #private }
RBParseTreeTest >> parseFaultyMethod: text [
	^RBParser parseFaultyMethod: text.
]

{ #category : #helpers }
RBParseTreeTest >> parseMethod: aString [ 

	^ RBParser parseMethod: aString
]

{ #category : #helpers }
RBParseTreeTest >> parseRewriteMethod: aString [ 

	^ RBParser parseRewriteMethod: aString
]
