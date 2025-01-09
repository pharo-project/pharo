Class {
	#name : 'OCParseTreeTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'testing' }
OCParseTreeTest class >> isAbstract [

	^self name = #OCParseTreeTest
]

{ #category : 'helpers' }
OCParseTreeTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : 'helpers' }
OCParseTreeTest >> parseFaultyExpression: aString [
	^ self parserClass parseFaultyExpression: aString
]

{ #category : 'private' }
OCParseTreeTest >> parseFaultyMethod: text [
	^ self parserClass parseFaultyMethod: text
]

{ #category : 'helpers' }
OCParseTreeTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : 'helpers' }
OCParseTreeTest >> parseRewriteMethod: aString [
	^ self parserClass parseRewriteMethod: aString
]

{ #category : 'helpers' }
OCParseTreeTest >> parserClass [
	^ OCParser
]
