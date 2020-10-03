Class {
	#name : #RBErrorNodeParserTest,
	#superclass : #RBParserTest,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ arg'.
	
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #arg.
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ 12'.
	
	self assert: node isSequence.
	
	self assert: node statements first isUnfinishedStatement.
	self assert: node statements first statement isParseError.
	
	self assert: node statements second isLiteralNode.
	self assert: node statements second value equals: 12.

]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: arg'.
	
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #arg.
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: 12'.
	
	self assert: node isSequence.
	
	self assert: node statements first isParseError.

	self assert: node statements second isLiteralNode.
	self assert: node statements second value equals: 12.

]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendWithSymbolsArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: #lala: and:12'.
	
	self assert: node isSequence.
	self assert: node statements first isUnfinishedStatement.
	self assert: node statements first statement isParseError.

	self assert: node statements second isMessage.
	self assert: node statements second receiver value equals: #lala:.
	self assert: node statements second selector equals: #and:.
	self assert: node statements second arguments first isLiteralNode.
	self assert: node statements second arguments first value equals: 12.

]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMethodConsumesEntireStream [

	| parser |
	parser := self parserClass newFaulty.
	parser initializeParserWith: 'ret ^ 1 ^ 2'.
	parser parseMethod.
	
	self assert: parser atEnd
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMethodWithSignatureKeepsSignature [

	| node |
	node := self parserClass parseFaultyMethod: 'ret ^ 1 ^ 2'.
	
	self assert: node isMethod.
	self assert: node selector equals: #ret
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMethodWithoutSignatureHasAnErrorNodeAndContinueParsing [

	| node |
	node := self parserClass parseFaultyMethod: '1 between: 2 and: 3'.
	
	self assert: node isMethod.
	self assert: node selector equals: #faulty.

	self assert: node body statements first isParseError.

	self assert: node body statements second isMessage.
	self assert: node body statements second selector equals: #between:and:.

]
