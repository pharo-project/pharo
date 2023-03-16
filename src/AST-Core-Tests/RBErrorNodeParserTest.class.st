Class {
	#name : #RBErrorNodeParserTest,
	#superclass : #RBParserTest,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyAssigment [

	| node |
	node := self parserClass parseFaultyExpression: ':=1'.

	self assert: node isFaulty.
	self assert: node isParseError.
	self   deny: node contents first isFaulty.
	self assert: node contents first value equals: 1.
	self assert: node sourceInterval equals: (1 to: 3).
	self assert: node errorMessage equals: 'identifier expected in assigment'.
	self assert: node formattedCode equals: ':= 1'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyAssigment2 [

	| node |
	node := self parserClass parseFaultyExpression: 'a:='.

	self assert: node isFaulty.
	self assert: node value isParseError.
	self assert: node value sourceInterval equals: (4 to: 3).
	self assert: node value errorMessage equals: 'Variable or expression expected'.
	self assert: node value formattedCode equals: ''.
	self assert: node formattedCode equals: 'a := '
]

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
RBErrorNodeParserTest >> testFaultyBlockUnclosed [

	| node |
	node := self parserClass parseFaultyExpression: '[1+2'.

	self assert: node isEnglobingError.
	self assert: node isBlockError.
	self assert: node errorMessage equals: ''']'' expected'.
	self assert: node formattedCode equals: '[ 1 + 2'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyBlockUnopeneed [

	| node |
	node := self parserClass parseFaultyExpression: '1+2]'.

	self assert: node isEnglobingError.
	self assert: node isUnfinishedStatement.
	self assert: node errorMessage equals: 'Missing opener for closer: ]'.
	self assert: node formattedCode equals: '1 + 2 ]'
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
RBErrorNodeParserTest >> testFaultyCascadeCascadeMessageExpected [

	| node |
	node := self parserClass parseFaultyExpression: '1 foo;'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self deny: node messages first isFaulty.
	self assert: node messages first selector equals: #foo.

	self assert: node messages second isParseError.
	self assert: node messages second sourceInterval equals: (7 to: 6).
	self
		assert: node messages second errorMessage
		equals: 'Cascade message expected'.
	self assert: node messages second formattedCode equals: ''.

	self
		assert: node formattedCode
		equals: '1<r><t>foo;<r><t>' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeCascadeMessageExpected2 [

	| node |
	node := self parserClass parseFaultyExpression: '1 foo;;bar'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self deny: node messages first isFaulty.
	self assert: node messages first selector equals: #foo.

	self assert: node messages second isParseError.
	self assert: node messages second sourceInterval equals: (7 to: 6).
	self
		assert: node messages second errorMessage
		equals: 'Cascade message expected'.
	self assert: node messages second formattedCode equals: ''.

	self deny: node messages third isFaulty.
	self assert: node messages third selector equals: #bar.

	self
		assert: node formattedCode
		equals: '1<r><t>foo;<r><t>;<r><t>bar' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeCascadeMessageExpected3 [

	| node |
	node := self parserClass parseFaultyExpression: '1 foo;;'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self deny: node messages first isFaulty.
	self assert: node messages first selector equals: #foo.

	self assert: node messages second isParseError.
	self assert: node messages second sourceInterval equals: (7 to: 6).
	self
		assert: node messages second errorMessage
		equals: 'Cascade message expected'.
	self assert: node messages second formattedCode equals: ''.

	self assert: node messages third isParseError.
	self assert: node messages third sourceInterval equals: (8 to: 7).
	self
		assert: node messages third errorMessage
		equals: 'Cascade message expected'.
	self assert: node messages third formattedCode equals: ''.

	self
		assert: node formattedCode
		equals: '1<r><t>foo;<r><t>;<r><t>' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeMessageExpected [

	| node |
	node := self parserClass parseFaultyExpression: ';foo'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self assert: node messages first isCascadeError.
	self assert: node messages first sourceInterval equals: (1 to: 1).
	self
		assert: node messages first errorMessage
		equals: 'Message expected'.
	self assert: node messages first formattedCode equals: ''.

	self assert: node messages second isFaulty. "Faulty because there is no receiver!"
	self assert: node messages second selector equals: #foo.
	self assert: node messages second receiver isParseError.

	self
		assert: node formattedCode
		equals: '<r><t>;<r><t>foo' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeMessageExpected2 [

	| node |
	node := self parserClass parseFaultyExpression: ';;foo'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self assert: node messages first isCascadeError.
	self assert: node messages first sourceInterval equals: (1 to: 1).
	self
		assert: node messages first errorMessage
		equals: 'Message expected'.
	self assert: node messages first formattedCode equals: ''.

	self assert: node messages second isParseError.
	self assert: node messages second sourceInterval equals: (2 to: 1).
	self
		assert: node messages second errorMessage
		equals: 'Cascade message expected'.
	self assert: node messages second formattedCode equals: ''.

	self assert: node messages third isFaulty. "Faulty because there is no receiver!"
	self assert: node messages third selector equals: #foo.
	self assert: node messages third receiver isParseError.

	self
		assert: node formattedCode
		equals: '<r><t>;<r><t>;<r><t>foo' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeMessageExpected3 [

	| node |
	node := self parserClass parseFaultyExpression: '(1 foo); bar'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self assert: node messages first isCascadeError.
	self assert: node messages first sourceInterval equals: (1 to: 8).
	self
		assert: node messages first errorMessage
		equals: 'Message expected'.
	self assert: node messages first formattedCode equals: '1 foo'.

	self deny: node messages first receiver isFaulty.
	self assert: node messages first receiver isMessage.
	self assert: node messages first receiver selector equals: #foo.

	self deny: node messages second isFaulty.
	self assert: node messages second isMessage.
	self assert: node messages second selector equals: #bar.

	self
		assert: node formattedCode
		equals: '1 foo<r><t>;<r><t>bar' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyCascadeMessageExpected4 [

	| node |
	node := self parserClass parseFaultyExpression: '1; bar'.

	self assert: node isFaulty.
	self assert: node isCascade.

	self assert: node messages first isCascadeError.
	self assert: node messages first sourceInterval equals: (1 to: 2).
	self
		assert: node messages first errorMessage
		equals: 'Message expected'.
	self assert: node messages first formattedCode equals: '1'.

	self deny: node messages first receiver isFaulty.
	self assert: node messages first receiver value equals: 1.

	self deny: node messages second isFaulty.
	self assert: node messages second isMessage.
	self assert: node messages second selector equals: #bar.

	self
		assert: node formattedCode
		equals: '1<r><t>;<r><t>bar' expandMacros
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyLiterabByteArray [

	| source node |
	source := '#[ 1 hello 3 ]'.
	node := self parserClass parseFaultyExpression: source.

	self assert: node isFaulty.
	self assert: node isLiteralByteArrayError.

	self deny: node contents first isFaulty.
	self assert: node contents first value equals: 1.

	self assert: node contents second isFaulty.
	self assert: node contents second value equals: 'hello'.
	self assert: node contents second sourceInterval equals: (6 to: 10).
	self assert: node contents second errorMessage equals: '8-bit integer expected'.
	self assert: node contents second formattedCode equals: 'hello'.

	self deny: node contents third isFaulty.
	self assert: node contents third value equals: 3.

	self assert: node sourceInterval equals: (1 to: 14).
	self assert: node formattedCode equals: source
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
	self assert: node selector equals: #''.

	self assert: node body statements first isParseError.

	self assert: node body statements second isMessage.
	self assert: node body statements second selector equals: #between:and:
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyNumericLiteral [

	| node |
	node := self parserClass parseFaultyExpression: '000r'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (1 to: 4).
	self assert: node receiver errorMessage equals: 'an integer greater than 1 as valid radix expected'.
	self assert: node selector equals: #r.
	self assert: node receiver formattedCode equals: '000'.
	self assert: node formattedCode equals: '000 r'
]

{ #category : #tests }
RBErrorNodeParserTest >> testFaultyNumericLiteral2 [

	| node |
	node := self parserClass parseFaultyExpression: '2rx'.

	self assert: node isFaulty.
	self assert: node isMessage.
	self assert: node receiver isParseError.
	self assert: node receiver sourceInterval equals: (1 to: 3).
	self assert: node receiver errorMessage equals: 'a digit between 0 and 1 expected'.
	self assert: node selector equals: #x.
	self assert: node receiver formattedCode equals: '2r'.
	self assert: node formattedCode equals: '2r x'
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
	self assert: node arguments first sourceInterval equals: (7 to: 11).
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
	self assert: node arguments first sourceInterval equals: (7 to: 8).
	self assert: node arguments first formattedCode equals: ''''.
	self assert: node formattedCode equals: '''foo'' , '''.
]
