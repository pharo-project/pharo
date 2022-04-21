Class {
	#name : #ASTErrorNodeParserTest,
	#superclass : #ASTParserTest,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyBinaryMessageSendArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ arg'.
	
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #+.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyBinaryMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ 12'.
	
	self assert: node isMessage.
	
	self assert: node receiver isParseError.
	
	self assert: node arguments first isLiteralNode.
	self assert: node arguments first value equals: 12.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyCascade [

	| node |
	node := self parserClass parseFaultyExpression: '; ]'.
	
	self assert: node isUnfinishedStatement.
	self assert: node children first isCascade.
	self assert: node children first messages first isParseError.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMessageSendShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: arg'.
	
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #msg:.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: 12'.
	
	self assert: node isMessage.
	self assert: node selector equals: #msg:.
	
	self assert: node receiver isParseError.

	self assert: node arguments first isLiteralNode.
	self assert: node arguments first value equals: 12.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMessageSendWithSymbolsArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: #lala: and:12'.
	
	self assert: node isMessage.
	self assert: node selector equals: #msg:and:.
	
	self assert: node receiver isParseError.

	self assert: node arguments first value equals: #lala:.
	self assert: node arguments second isLiteralNode.
	self assert: node arguments second value equals: 12.
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMethodConsumesEntireStream [

	| parser |
	parser := self parserClass newFaulty.
	parser initializeParserWith: 'ret ^ 1 ^ 2'.
	parser parseMethod.
	
	self assert: parser atEnd
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMethodWithSignatureKeepsSignature [

	| node |
	node := self parserClass parseFaultyMethod: 'ret ^ 1 ^ 2'.
	
	self assert: node isMethod.
	self assert: node selector equals: #ret
]

{ #category : #tests }
ASTErrorNodeParserTest >> testFaultyMethodWithoutSignatureHasAnErrorNodeAndContinueParsing [

	| node |
	node := self parserClass parseFaultyMethod: '1 between: 2 and: 3'.
	
	self assert: node isMethod.
	self assert: node selector equals: #faulty.

	self assert: node body statements first isParseError.

	self assert: node body statements second isMessage.
	self assert: node body statements second selector equals: #between:and:.
]
