Class {
	#name : #RBErrorNodeParserTest,
	#superclass : #RBParserTest,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendArgument [

	| node |
	node := self parserClass parseFaultyExpression: '12 +'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node arguments first isParseError.
	self assert: node arguments first sourceInterval equals: (5 to: 4).
	self assert: node arguments first errorMessage equals: 'Variable or expression expected'.
	self assert: node selector equals: #+.
	self   deny: node receiver isFaulty.
	self assert: node receiver value equals: 12.
	self assert: node arguments first formattedCode equals: ''.
	self assert: node formattedCode equals: '12 + '
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendArgumentInside [

	| node0 node |
	node0 := self parserClass parseFaultyExpression: '[12 +]'.
	node := node0 body statements first.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node arguments first isParseError.
	self assert: node arguments first sourceInterval equals: (6 to: 5).
	self assert: node arguments first errorMessage equals: 'Variable or expression expected'.
	self assert: node selector equals: #+.
	self   deny: node receiver isFaulty.
	self assert: node receiver value equals: 12.
	self assert: node arguments first formattedCode equals: ''.
	self assert: node formattedCode equals: '12 + '.
	self assert: node0 formattedCode equals: '[ 12 +  ]'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ arg'.

	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #+
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendReceiver [

	| node |
	node := self parserClass parseFaultyExpression: '+ 12'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (1 to: 0).
	self assert: node receiver errorMessage equals: 'Variable or expression expected'.
	self assert: node selector equals: #+.
	self   deny: node arguments first isFaulty.
	self assert: node arguments first value equals: 12.
	self assert: node receiver formattedCode equals: ''.
	self assert: node formattedCode equals: ' + 12'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendReceiverInside [

	| node0 node |
	node0 := self parserClass parseFaultyExpression: '[+ 12]'.
	node := node0 body statements first.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (2 to: 1).
	self assert: node receiver errorMessage equals: 'Variable or expression expected'.
	self assert: node selector equals: #+.
	self   deny: node arguments first isFaulty.
	self assert: node arguments first value equals: 12.
	self assert: node receiver formattedCode equals: ''.
	self assert: node formattedCode equals: ' + 12'.
	self assert: node0 formattedCode equals: '[  + 12 ]'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBinaryMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: '+ 12'.

	self assert: node isMessage.

	self assert: node receiver isParseError.

	self assert: node arguments first isLiteralNode.
	self assert: node arguments first value equals: 12
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascade [

	| node |
	node := self parserClass parseFaultyExpression: '; ]'.

	self assert: node isUnfinishedStatement.
	self assert: node children first isCascade.
	self assert: node children first messages first isParseError
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: arg'.

	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node selector equals: #msg:
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendWithLiteralArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: 12'.

	self assert: node isMessage.
	self assert: node selector equals: #msg:.

	self assert: node receiver isParseError.

	self assert: node arguments first isLiteralNode.
	self assert: node arguments first value equals: 12
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyMessageSendWithSymbolsArgumentShouldHaveTheCorrectMessage [

	| node |
	node := self parserClass parseFaultyExpression: 'msg: #lala: and:12'.

	self assert: node isMessage.
	self assert: node selector equals: #msg:and:.

	self assert: node receiver isParseError.

	self assert: node arguments first value equals: #lala:.
	self assert: node arguments second isLiteralNode.
	self assert: node arguments second value equals: 12
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
	self assert: node body statements second selector equals: #between:and:
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyNumericLiteral [

	| node |
	node := self parserClass parseFaultyExpression: '000r+12'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (1 to: 4).
	self assert: node receiver errorMessage equals: 'an integer greater than 1 as valid radix expected'.
	self assert: node selector equals: #+.
	self assert: node receiver formattedCode equals: '000r'.
	self assert: node formattedCode equals: '000r + 12'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyNumericLiteral2 [

	| node |
	node := self parserClass parseFaultyExpression: '2r+12'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (1 to: 2).
	self assert: node receiver errorMessage equals: 'a digit between 0 and 1 expected'.
	self assert: node selector equals: #+.
	self assert: node receiver formattedCode equals: '2r'.
	self assert: node formattedCode equals: '2r + 12'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyStringLiteral [

	| node |
	node := self parserClass parseFaultyExpression: '''foo'',''bar'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node selector equals: #','.
	self assert: node arguments first isParseError.
	self assert: node arguments first errorMessage equals: 'Unmatched '' in string literal.'.
	self assert: node arguments first sourceInterval equals: (7 to: 10).
	self assert: node arguments first formattedCode equals: '''bar'.
	self assert: node formattedCode equals: '''foo'' , ''bar'.
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyStringLiteral2 [

	| node |
	node := self parserClass parseFaultyExpression: '''foo'','''.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node selector equals: #','.
	self assert: node arguments first isParseError.
	self assert: node arguments first errorMessage equals: 'Unmatched '' in string literal.'.
	self assert: node arguments first sourceInterval equals: (7 to: 7).
	self assert: node arguments first formattedCode equals: ''''.
	self assert: node formattedCode equals: '''foo'' , '''.
]
