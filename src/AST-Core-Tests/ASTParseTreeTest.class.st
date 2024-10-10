Class {
	#name : 'ASTParseTreeTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'testing' }
ASTParseTreeTest class >> isAbstract [

	^self name = #ASTParseTreeTest
]

{ #category : 'helpers' }
ASTParseTreeTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : 'helpers' }
ASTParseTreeTest >> parseFaultyExpression: aString [
	^ self parserClass parseFaultyExpression: aString
]

{ #category : 'private' }
ASTParseTreeTest >> parseFaultyMethod: text [
	^ self parserClass parseFaultyMethod: text
]

{ #category : 'helpers' }
ASTParseTreeTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : 'helpers' }
ASTParseTreeTest >> parseRewriteMethod: aString [
	^ self parserClass parseRewriteMethod: aString
]

{ #category : 'helpers' }
ASTParseTreeTest >> parserClass [
	^ RBParser
]
