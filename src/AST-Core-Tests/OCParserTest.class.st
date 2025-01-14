"
SUnit tests for RBParser
"
Class {
	#name : 'OCParserTest',
	#superclass : 'OCParseTreeTest',
	#category : 'AST-Core-Tests-Parser',
	#package : 'AST-Core-Tests',
	#tag : 'Parser'
}

{ #category : 'accessing' }
OCParserTest class >> parserClass [
	^ OCParser
]

{ #category : 'accessing' }
OCParserTest >> compare: anObject to: anotherObject [
	self assert: anObject hash equals: anotherObject hash.
	self assert: anObject equals: anotherObject
]

{ #category : 'accessing' }
OCParserTest >> exampleClasses [
	<sampleInstance>
	^ Array
		with: self parserClass
		with: OCScanner
		with: OCProgramNode
		with: OCCommentNode
]

{ #category : 'private' }
OCParserTest >> parseError: each [

	self parseError: each first onError: [ :string :pos | ^ self assert: pos equals: each last ].
	self assert: false description: 'Parser didn''t fail'
]

{ #category : 'private' }
OCParserTest >> parseError: expression onError: aBlock [
	^ self parserClass parseExpression: expression onError: aBlock
]

{ #category : 'private' }
OCParserTest >> parseFaultyExpression: text [
	^ self parserClass parseFaultyExpression: text
]

{ #category : 'tests' }
OCParserTest >> parseTreeSearcher [
	^ OCParseTreeSearcher new
]

{ #category : 'private' }
OCParserTest >> parserClass [
	^ self class parserClass
]

{ #category : 'tests - methodClass' }
OCParserTest >> testASTHasClass [

	| ast |
	ast := OCParser parseMethod: (Point>>#degrees) sourceCode.
	ast methodClass: Point. 
	self deny: ast methodClass isNil.
	self assert: ast methodClass equals: Point
]

{ #category : 'tests - parsing' }
OCParserTest >> testArray [
	| tree |
	#(('{}' 0 0) ('{.}' 0 1) ('{..}' 0 2) ('{foo. bar}' 2 1) ('{foo. bar.}' 2 2) ('{foo. bar. .}' 2 3) ('{. foo. bar}' 2 2) ('{foo.. bar}' 2 2))
	 do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size equals: each second.
		self assert: tree periods size equals: each last.
		self assert: tree left equals: 1.
		self assert: tree right equals: each first size ]
]

{ #category : 'tests' }
OCParserTest >> testArrayNodesArentArrays [
"Although an ASTArrayNode represents an array, it is not an Array-like object.
Therefore it shouldn't return true for message #isArray (like an instance of a real
array would"
	| arrayNode nonArrayNode |
	arrayNode := self parserClass parseExpression: '{ 1 . 2 . 3}'.

	nonArrayNode := self parserClass parseExpression: 'a := 3'.

	self deny: arrayNode isArray.
	self assert: arrayNode isDynamicArray.
	self deny: nonArrayNode isDynamicArray
]

{ #category : 'tests - Array' }
OCParserTest >> testArrayNodesContainRightAmountOfStatementsAndPeriods [
	"1. The array to be parsed into an array node.
	 2. The number of statements in the array node.
	 3. The number of periods in the array node."
	| tree |
	#(('{}' 0 0)
	  ('{.}' 0 1)
	  ('{..}' 0 2)
	  ('{foo. bar}' 2 1)
	  ('{foo. bar.}' 2 2)
	  ('{foo. bar. .}' 2 3)
	  ('{. foo. bar}' 2 2)
	  ('{foo.. bar}' 2 2))
	do: [ :each | tree := self parserClass parseExpression: each first.
			self assert: tree statements size equals: each second.
			self assert: tree periods size equals: each last. ]
]

{ #category : 'garbage tests' }
OCParserTest >> testAssignToMessageRaisesSyntaxError [
	"Raising a syntax exception when trying to do an assignement on a message."
	self should: [self parserClass parseExpression: 'receiver selector := 1 + 2'] raise: OCCodeError
]

{ #category : 'garbage tests' }
OCParserTest >> testAssignementRightValueWithUnfortunatePontuationRaisesSyntaxError [
	"The idea is for the parser to realise the first dot isn't supposed to be here and select it. Also add a side note to give suggestion     on the provenance of the error."
	self should: [self parserClass parseExpression: 'temp := .1 + 2.'] raise: OCCodeError.
	self should: [self parserClass parseExpression: 'temp := 1. + 2.'] raise: OCCodeError.
	self should: [self parserClass parseExpression: 'temp := 1 +. 2.'] raise: OCCodeError.

	"The same could be made for other special characters."
	self should: [self parserClass parseExpression: 'temp := ;1 + 2.'] raise: OCCodeError.
	self should: [self parserClass parseExpression: 'temp := ,1 + 2.'] raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testBadlyPlacedPragmaRaiseError [
	"The placement in the string will have an impact on the parsing methods involved.
	 Pragmas are supposed to be written in the beginning of a method."
	self shouldnt: [(self parserClass parseMethod: 'foo | token | <some: #tag> ^true')]
		  raise: OCCodeError.

	self should: [(self parserClass parseMethod: 'foo | token | ^true <some: #tag>')]
		  raise: 	OCCodeError.

	self should: [(self parserClass parseMethod: 'foo | token | token:= 5 <some: #tag> ^token')]
		  raise: 	OCCodeError.

	self should: [(self parserClass parseMethod: 'foo | token |  token messaged <some: #tag> ^token')]
		  raise: 	OCCodeError
]

{ #category : 'tests - Interval' }
OCParserTest >> testBestNodeFor [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo:
			[:each |
			each sourceInterval isEmpty
				ifFalse:
					[self assert: ((tree bestNodeFor: each sourceInterval) = each
								or: [each parent isCascade and: [each parent messages last = each]])]]
]

{ #category : 'tests - parsing' }
OCParserTest >> testBinarySelectors [
	"ANSI Smalltalk Standard Draft, page 32:
		binaryCharacter ::=
			'!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
		binarySelector ::= binaryCharacter+"

	| binaryCharacters selectorStrings |
	"Create a collection of strings to parse.
	 The collection is made of every possible string sized from 1 to 3 selectors."
	binaryCharacters := '!%&*+,/<=>?@\~-'.
	selectorStrings := OrderedCollection new.
	binaryCharacters do: [:first |
		selectorStrings addLast: (String with: first).
		binaryCharacters do: [:second |
			selectorStrings addLast: (String with: first with: second).
			binaryCharacters do: [:third |
				selectorStrings addLast: (String with: first with: second with: third)]]].

	""
	selectorStrings do: [:each | | methodNode messageNode |
		methodNode := self parseMethod: each, 'anObject
	^self',each,'anObject'.
		self assert: methodNode selector equals: each asSymbol.
		self assert: methodNode argumentNames asArray equals: #(anObject).
		self assert: methodNode statements size equals: 1.

		messageNode := methodNode statements first value.
		self assert: messageNode isMessage.
		self assert: messageNode receiver name equals: #self.
		self assert: messageNode selector equals: each asSymbol.
		self assert: messageNode arguments size equals: 1.
		self assert: messageNode arguments first isValue.
		self assert: messageNode arguments first name equals: #anObject]
]

{ #category : 'tests' }
OCParserTest >> testBlockReturnNode [
	| tree |
	" no explicit return"
	tree := self parserClass parseMethod:
			'tmp [ 3+4 ]'.
	self deny: tree lastIsReturn.
	" no explicit return the block local return should not count as method return "
	tree := self parserClass parseMethod:
	      'tmp [^ 3+4 ]'.
	self deny: tree lastIsReturn.

	" explicit return"
	tree := self parserClass parseMethod:
			'tmp ^ [ 3+4 ]'.
	self assert: tree lastIsReturn
]

{ #category : 'tests - parsing' }
OCParserTest >> testCascade [
	self assert: (self parserClass parseExpression: ' self msg; yourself') isCascade.
	self assert: (self parserClass parseExpression: ' self msg:(arg msg:arg); yourself') isCascade.
	self should: [ self parserClass parseExpression: ' (self msg); yourself' ] raise: OCCodeError.
	self should: [ self parserClass parseExpression: ' self ; yourself' ] raise: OCCodeError.
	self should: [ self parserClass parseExpression: ' (self) ; yourself' ] raise: OCCodeError
]

{ #category : 'tests - Cascade' }
OCParserTest >> testCascadeReplacement [
	"Cascade is composed of an ordered collection of messages, each with the same receiver."
	| cascade |
	cascade := self parserClass parseExpression: 'self foo; bar; baz'.
	"The replacement is made on the collection."
	(cascade messages at: 2)
		replaceWith: (self parserClass parseExpression: 'self bar: 2').
	self assert: cascade
		  equals: (self parserClass parseExpression: 'self foo; bar: 2; baz').

	"The replacement can be a message with another receiver"
	(cascade messages at: 3)
		replaceWith: (self parserClass parseExpression: 'notself barataba').
	self assert: (cascade messages at: 3) receiver
		  equals: (self parserClass parseExpression: 'notself')
]

{ #category : 'error testing' }
OCParserTest >> testCascadeReplacementCanLeadToOddErrors [
	| cascade |
	cascade := self parserClass parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2)
		replaceWith: (self parserClass parseExpression: '2').

	cascade := self parserClass parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2)
		replaceWith: (self parserClass parseExpression: 'temp := temp bar').

	"This tests create an error in the printing of self and parent of the node."
]

{ #category : 'error testing' }
OCParserTest >> testCascadeWithInvalidSelectorRaisesError [
	| tree0 tree |
	tree0 := self parseFaultyExpression: 'faulty cascade; repared ; 1; node'.
	self assert: tree0 isSequence.

	tree := tree0 statements first.
	self assert: tree isUnfinishedStatement.

	tree := tree statement.
	self assert: tree isCascade.
	self assert: tree messages size equals: 3.

	"The presence of an error in a cascade (when parsingFaulty) aborts the cascade and start a new statement.
	The error node is stored inside the cascade."
	self assert: tree messages first isMessage.
	self assert: tree messages first receiver name equals: #faulty.
	self assert: tree messages first selectorNode equals: (OCSelectorNode value: #cascade).

	self assert: tree messages second isMessage.
	self assert: tree messages second receiver name equals: #faulty.
	self assert: tree messages second selectorNode equals: (OCSelectorNode value: #repared).

	self assert: tree messages third isParseError.
	self assert: tree messages third errorMessage equals: 'Cascade message expected'.

	"Its possible that what follows the cascade is also a cascade, but with a faulty first message.
	The error is also stored inside the cascade."

	tree := tree0 statements second.
	self assert: tree isCascade.
	self assert: tree messages size equals: 2.

	self assert: tree messages first isParseError.
	self assert: tree messages first errorMessage equals: 'Message expected'.

	self assert: tree messages last isMessage.
	self assert: tree messages last receiver value equals: 1.
	self assert: tree messages last selectorNode equals: (OCSelectorNode value: #node)
]

{ #category : 'error testing' }
OCParserTest >> testCascadeWithMissingSelectorRaisesError [
	| tree |
	tree := self parseFaultyExpression: 'faulty cascade; repared ;; node'.
	self assert: tree isCascade.
	self assert: tree messages size equals: 4.

	"The presence of an error in a cascade (when parsingFaulty) parses the cascade until the end and stores the error node inside 	 the cascade."
	self assert: tree messages first isMessage.
	self assert: tree messages first receiver name equals: #faulty.
	self assert: tree messages first selectorNode equals: (OCSelectorNode value: #cascade).

	self assert: tree messages second isMessage.
	self assert: tree messages second receiver name equals: #faulty.
	self assert: tree messages second selectorNode equals: (OCSelectorNode value: #repared).

	self assert: tree messages third isFaulty.
	self assert: tree messages third errorMessage equals: 'Cascade message expected'.

	self assert: tree messages last isMessage.
	self assert: tree messages last receiver name equals: #faulty.
	self assert: tree messages last selectorNode equals: (OCSelectorNode value: #node)
]

{ #category : 'error testing' }
OCParserTest >> testCascadeWithNoMessageRaiseError [
	self should: [ self parserClass parseExpression: ' (self msg); yourself' ] raise: OCCodeError.
	self should: [ self parserClass parseExpression: ' self ; yourself' ] raise: OCCodeError.
	self should: [ self parserClass parseExpression: ' (self) ; yourself' ] raise: OCCodeError.
	self should: [ self parserClass parseExpression: ' self msg ;  ; yourself' ] raise: OCCodeError
]

{ #category : 'tests - comparing' }
OCParserTest >> testComparingTrees [
	self compare: self treeWithEverything to: self treeWithEverything.
	self compare: self treeWithReallyEverything to: self treeWithReallyEverything.

	self exampleClasses do: [ :class |
		class selectors do: [ :selector |
			self
				compare: (class parseTreeForSelector: selector)
				to: (class parseTreeForSelector: selector) ] ]
]

{ #category : 'tests' }
OCParserTest >> testCopy [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: tree copy.
	tree := self treeWithReallyEverything.
	self compare: tree to: tree copy.

	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeForSelector: each.
			self compare: tree to: tree copy ] ]
]

{ #category : 'tests' }
OCParserTest >> testCopyInContext [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).

	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeForSelector: each.
			self compare: tree to: (tree copyInContext: Dictionary new) ] ]
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationBlockNodeWithExpression [
	self compare: (self parserClass parseExpression: '[:a :b | a messaged. b messaged.]')
		  to:(OCBlockNode
			   arguments: (OrderedCollection with: (OCVariableNode named: 'a') with: (OCVariableNode named: 'b'))
				body: (OCSequenceNode statements: (OrderedCollection with: (OCMessageNode receiver: (OCVariableNode named: #a) selector: (OCSelectorNode value: #messaged))
																						with: (OCMessageNode receiver: (OCVariableNode named: #b) selector: (OCSelectorNode value: #messaged)))))
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationCascadeNodeFromExpression [
	self compare: (self parserClass parseExpression: 'cascade firstMessage; secondMessage; lastMessage')
		  to: (OCCascadeNode
				messages: (OrderedCollection with: (OCMessageNode receiver: (OCVariableNode named: 'cascade')
																					selector: (OCSelectorNode value: #firstMessage))
													  with: (OCMessageNode receiver: (OCVariableNode named: 'cascade')
																					selector: (OCSelectorNode value: #secondMessage))
													  with: (OCMessageNode receiver: (OCVariableNode named: 'cascade')
																					selector: (OCSelectorNode value: #lastMessage)))
				semicolons: (OrderedCollection with: 21 with: 37))
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationLiteralArrayNodeWithExpression [
	self compare: (self parserClass parseExpression: '#(literal1 literal2)')
		  to: (OCLiteralArrayNode
				startPosition: 1
				contents: (Array with: (OCLiteralValueNode value: #literal1) with: (OCLiteralValueNode value: #literal2))
				stopPosition: 20
				isByteArray: false)
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationLiteralValueNodeWithExpression [
	self compare: (self parserClass parseExpression: '#literal')
		  to: (OCLiteralValueNode
				value: #literal
				start: 1
				stop: 8)
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationLiteralValueNodeWithExpressionForNumber [
	self compare: (self parserClass parseExpression: '2r10100')
		  to: (OCLiteralValueNode
				value: 20
				start: 1
				stop: 7)
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationMessageNodeFromExpression [
	"Verify the good parsing of an expression."
	self compare: (self parserClass parseExpression: 'self + 0')
		  to: (OCMessageNode
				receiver: (OCVariableNode named: 'self')
				selector: (OCSelectorNode value: #+)
				arguments: (Array with: (OCLiteralNode value: 0)))
]

{ #category : 'tests' }
OCParserTest >> testCreationProtocol [
	| messageNode |
	self compare: (OCMessageNode
				receiver: (OCVariableNode selfNode)
				selector: (OCSelectorNode value: #+)
				arguments: (Array with: (OCLiteralNode value: 0)))
		to: (self parserClass parseExpression: 'self + 0').
	messageNode := OCMessageNode receiver: (OCVariableNode selfNode) selector: (OCSelectorNode value: #foo).
	self compare: (OCMethodNode selector: #bar
				body: (OCSequenceNode statements: (OrderedCollection
								with: (OCCascadeNode messages: (OrderedCollection with: messageNode with: messageNode)))))
		to: (self parserClass parseMethod: 'bar self foo; foo')
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationSelfNodeWithExpression [
	self compare: (self parserClass parseExpression: 'self')
		  to: OCVariableNode selfNode
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationSuperNodeWithExpression [
	self compare: (self parserClass parseExpression: 'super')
		  to: OCVariableNode superNode
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationThisContexNodeWithExpression [
	self compare: (self parserClass parseExpression: 'thisContext')
		  to: OCVariableNode thisContextNode
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationThisProcessNodeWithExpression [
	self compare: (self parserClass parseExpression: 'thisProcess')
		  to: OCVariableNode thisProcessNode 
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testCreationVariableNodeWithExpression [
	self compare: (self parserClass parseExpression: 'variable')
		  to: (OCVariableNode named: #variable start: 1)
]

{ #category : 'tests - Array' }
OCParserTest >> testDelimitationsOfArrayNodesAreBrackets [
	| tree |
	#('{}' '{.}' '{..}' '{foo. bar}' '{foo. bar.}' '{foo. bar. .}' '{. foo. bar}' '{foo.. bar}')
	do: [ :each |  tree := self parserClass parseExpression: each.
						self assert: tree left equals: 1.
						self assert: tree right equals: each size ]
]

{ #category : 'tests - comparing' }
OCParserTest >> testEqualToWithMapping [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).

	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeForSelector: each.
			self assert: (tree equalTo: tree withMapping: Dictionary new) ] ]
]

{ #category : 'tests - comparing' }
OCParserTest >> testEquivalentExceptRenaming [
	#(#('a 3-4' 'a 4-3' false) #('a #[3 4]' 'a #(3 4)' false) #('a variable1 ~~ "comment" variable2' 'a variable1 ~~ variable2' true) #('a variable1' 'a variable2' false) #('a [:a :b | a + b]' 'a [:b :a | a + b]' false) #('a | a b | a + b' 'a | b a | a + b' true) #('a | a | a msg1; msg2' 'a | b | b msg2; msg2' false) #('a c' 'a d' true) #('a | a b | a := b. ^b msg1' 'a | a b | b := a. ^a msg1' true) #('a | a b | a := b. ^b msg1: a' 'a | a b | b := a. ^b msg1: a' false) #('a: b b + 4' 'a: e e + 4' true) #('a: b b + 4' 'b: b b + 4' false) #('a: b b: c b + c' 'a: c b: b c + b' true) #('a: a b: b a + b' 'a: b b: a a + b' false))
		do: [ :each |
			self
				assert: ((self parserClass parseMethod: each first) equalTo: (self parserClass parseMethod: (each at: 2)) exceptForVariables: #('c'))
				identicalTo: each last ]
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesHasRightMessage [
	| tree |
	tree := self parseFaultyExpression: '( faulty parentheses message node'.
	self assert: tree isParseError.
	self assert: tree errorMessage equals: ''')'' expected'
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesIsEnglobing [
	| tree |
	tree := self parseFaultyExpression: '( faulty parentheses message node'.
	self assert: tree isEnglobingError
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesOfLiteralArrayIsEnglobingNode [
	| tree expr |
	expr := '#( faulty parentheses litteral array node'.
	tree := self parseFaultyExpression: expr.
	self assert: tree isEnglobingError
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesStartAtFirstParenthese [
	| tree |
	tree := self parseFaultyExpression: '( faulty parentheses message node'.
	self assert: tree isParseError.
	self assert: tree start equals: 1
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesStartAtLiteralArrayToken [
	| tree expr |
	expr := '#( faulty parentheses litteral array node'.
	tree := self parseFaultyExpression: expr.

	self assert: tree isParseError.
	self assert: tree errorMessage equals: ''')'' expected'.
	self assert: tree start equals: 1.
	self assert: tree stop equals: expr size
]

{ #category : 'error testing' }
OCParserTest >> testErrorNodeForBadParenthesesStopAtEnd [
	| tree expr |
	expr := '( faulty parentheses message node'.
	tree := self parseFaultyExpression: expr.
	self assert: tree isParseError.
	self assert: tree stop equals: expr size
]

{ #category : 'error testing' }
OCParserTest >> testFaultyLiteralRaiseSyntaxError [
	#('#"bar"foo' '# foo' '#1' '#12' '#12.3' '# 1' '##1' '#"bar"1')
		do: [ :expr |
		self should: [self parserClass parseExpression: expr] raise: OCCodeError ]
]

{ #category : 'tests' }
OCParserTest >> testFormatter [
	self exampleClasses do: [ :class |
		class selectors do: [ :selector |
		self
			compare: (class parseTreeForSelector: selector)
			to: (self parserClass parseMethod: (class parseTreeForSelector: selector) printString) ] ]
]

{ #category : 'garbage tests' }
OCParserTest >> testGarbageForgottenDotAfterAssignement [
	| tree child error |
	self shouldnt: [self parserClass parseExpression: '|temp | temp := 1 + 2 temp + 3'] raise: OCCodeError.

	tree := self parserClass parseExpression: '|temp | temp := 1 + 2 temp + 3'.
	self assert: tree isSequence.

	"There should only be one statement as there is no dot."
	child := tree statements.
	self assert: (child size) equals: 1.
	self assert: child first isAssignment.

	"Getting the part changed by missing dot."
	error := child first value receiver arguments first.
	self assert: error isMessage.
	self assert: error receiver value equals: 2.
	self assert: error selectorNode equals: (OCSelectorNode value: #temp)
]

{ #category : 'garbage tests' }
OCParserTest >> testGarbageForgottenReceiver [
	"Test made in sight of implementing the search of previous litterals when encountering an unknown
	 variable in case it is, in fact, a selector. This works for cascade (forgotten ;) or missing
	 receiver like self ..."
	self should: [self parserClass parseExpression: '|temp | temp := 1 + 2. + 3'] raise: OCCodeError
]

{ #category : 'garbage tests' }
OCParserTest >> testGarbageTemporaryVariable [
	| tree |
	tree := self parseFaultyExpression: '| temp'.
	self assert: tree isFaulty.
	self assert: tree errorMessage equals: '''|'' or variable expected'
]

{ #category : 'garbage tests' }
OCParserTest >> testGarbageTemporaryVariableHasMissingClosure [
	| tree |
	tree := self parseFaultyExpression: '| temp'.
	self assert: tree isFaulty.
	self assert: tree errorMessage equals: '''|'' or variable expected'
]

{ #category : 'garbage tests' }
OCParserTest >> testGarbageTemporaryVariableIsWrong [
	"The parser doesn't recognise the 2 because it is not an identifier."
	| tree errorNode |
	tree := self parseFaultyExpression: '| 2 |'.
	self assert: tree isFaulty.
	self assert: tree isSequence.

	errorNode := tree statements first.
	self assert: errorNode isParseError.
	self assert: errorNode errorMessage equals: '''|'' or variable expected'
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralBlockIntervalsArray [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').
	str := '{array node}'.
	self assert: ((tree body statements at: 4) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 4) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsAssignement [
	"This tests that assignement nodes inside a block node correspond the right interval in the source code."
	| tree str assignementNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := 'assignement := node'.
	assignementNode := tree body statements at: 7.
	position := tree source findString: str.

	self assert: assignementNode start equals: position.
	self assert: assignementNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsBlock [
	"This tests that block nodes inside a block node correspond the right interval in the source code."
	| tree str blockNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := '[block node]'.
	blockNode := tree body statements at: 5.
	position := tree source findString: str.

	self assert: blockNode start equals: position.
	self assert: blockNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsCascade [
	"This tests that cascade nodes inside a block node correspond the right interval in the source code."
	| tree str cascadeNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := 'cascade node; cascaded'.
	cascadeNode := tree body statements at: 6.
	position := tree source findString: str.

	self assert: cascadeNode start equals: position.
	self assert: cascadeNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsLiteralArray [
	"This tests that literal array nodes inside a block node correspond the right interval in the source code."
	| tree str literalArrayNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := '#(literal array node)'.
	literalArrayNode := tree body statements at: 3.
	position := tree source findString: str.

	self assert: literalArrayNode start equals: position.
	self assert: literalArrayNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsLiteralValue [
	"This tests that literal value nodes inside a block node correspond the right interval in the source code."
	| tree str literalValueNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := '#literalNode'.
	literalValueNode := tree body statements at: 2.
	position := tree source findString: str.

	self assert: literalValueNode start equals: position.
	self assert: literalValueNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsMessage [
	"This tests that message nodes inside a block node correspond the right interval in the source code."
	| tree str messageNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := 'message node'.
	messageNode := tree body statements at: 1.
	position := tree source findString: str.

	self assert: messageNode start equals: position.
	self assert: messageNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralBlockIntervalsReturn [
	"This tests that return nodes inside a block node correspond the right interval in the source code."
	| tree str returnNode position |
	tree := (self parseExpression: '["comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node]').

	str := '^return node'.
	returnNode := tree body statements at: 8.
	position := tree source findString: str.

	self assert: returnNode start equals: position.
	self assert: returnNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsArray [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '{array node}'.
	self assert: ((tree body statements at: 4) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 4) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsAssignement [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := 'assignement := node'.
	self assert: ((tree body statements at: 7) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 7) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsBlock [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '[block node]'.
	self assert: ((tree body statements at: 5) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 5) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsCascade [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := 'cascade node; cascaded'.
	self assert: ((tree body statements at: 6) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 6) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Intervals' }
OCParserTest >> testGeneralMethodIntervalsComment [
	"This tests that comment nodes inside a method node correspond the right interval in the source code."
	| tree str commentNode position |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').

	str := '"comment node"'.
	commentNode := tree comments at: 1.
	position := tree source findString: str.

	self assert: commentNode start equals: position.
	self assert: commentNode stop equals: position + (str size - 1)
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsLiteralArray [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '#(literal array node)'.
	self assert: ((tree body statements at: 3) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 3) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsLiteralValue [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '#literalNode'.
	self assert: ((tree body statements at: 2) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 2) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsMessage [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := 'message node'.
	self assert: ((tree body statements at: 1) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 1) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsPragma [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '<pragmanode>'.
	self assert: (tree pragmas first left) equals: (tree source findString: str).
	self assert: (tree pragmas first right) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Interval' }
OCParserTest >> testGeneralMethodIntervalsReturn [
	"This tests that all types of nodes in a method are well made."
	| tree str |
	tree := (self parseMethod: 'Selector <pragmanode> "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node').
	str := '^return node'.
	self assert: ((tree body statements at: 8) start) equals: (tree source findString: str).
	self assert: ((tree body statements at: 8) stop) equals: ((tree source findString: str) + (str size - 1))
]

{ #category : 'tests - Pragma' }
OCParserTest >> testIdentificationOfPrimitiveMethods [
	"Verify if there is the pragma primitive in the parsed method."
	"The string is the element to be parsed.
	 The boolean is the expected result. true : is primitive
													false : is not primitive."
	#(('foo ^true' false )
	  ('foo <some: #tag> ^true' false )
	  (' foo <some: #tag> <primitive: 123> ^self' true )
	  ('foo primitive:= 123. ^self' false))
	do: [:each | self assert: (self parserClass parseMethod: each first) isPrimitive equals: each last]
]

{ #category : 'tests - Interval' }
OCParserTest >> testIntervals [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo:
			[:each |
			(each parent isNil or: [each parent isCascade not and: [ each parent isLiteralNode not]])
				ifTrue:
					[| newNode source |
					source := tree source copyFrom: each start to: each stop.
					each isPragma ifFalse: [
						newNode := each isMethod
								ifTrue: [self parserClass parseMethod: source]
								ifFalse: [self parserClass parseExpression: source].
						self compare: each to: newNode]]]
]

{ #category : 'error testing' }
OCParserTest >> testInvalidPragmaPosition [
	#(
	'foo self. <foo: $a>'
	'<foo: $a>'
	'foo [:x | <foo: $a> ] value'
	'foo ^ <foo: $a>'
	)
	do: [ :each | self should: [ self parserClass parseMethod: each ] raise: OCCodeError ]
]

{ #category : 'garbage tests' }
OCParserTest >> testInvalidSelectorRaisesSyntaxError [
	"Could propose multiple reparations like assignement, period, array..."
	self should: [self parserClass parseExpression: 'temp 1 + 3'] raise: OCCodeError
]

{ #category : 'tests' }
OCParserTest >> testIsA [
	| nodes types |
	nodes := Bag new.
	types := Set new.
	#(#(#isAssignment 1) #(#isBlock 1) #(#isCascade 1) #(#isLiteralNode 2) #(#isMessage 3) #(#isMethod 1) #(#isReturn 1) #(#isSequence 2) #(#isValue 15) #(#isVariable 7) #(#isUsedAsReturnValue 10) #(#isEssential 12) #(#hasParentheses 1) #(#isBinary 0) #(#isPrimitive 0) #(#isImmediateNode 10) #(#isWrite 1) #(#isRead 3))
		do: [ :each |
			each last timesRepeat: [ nodes add: each first ].
			types add: each first ].
	self treeWithEverything nodesDo: [ :each | types do: [ :sel | ((each respondsTo: sel) and: [ each perform: sel ]) ifTrue: [ nodes remove: sel ] ] ].
	self assertEmpty: nodes
]

{ #category : 'tests - isUsed' }
OCParserTest >> testIsUsedWhenABlockNodeIsReturnedExpectIsUsed [
	| tree |
	"an explicit return node is *used*"
	tree := self parserClass parseMethod:
			'tmp ^[ 3+4 ]'.
	self assert: tree body statements last isUsedAsReturnValue
]

{ #category : 'tests - isUsed' }
OCParserTest >> testIsUsedWhenBlockAloneExpectIsNotUsed [
	| tree |
	" block node is not a used-node"
	tree := self parserClass parseMethod:
			'tmp [ 3+4 ]'.
	self deny: tree body statements last isUsedAsReturnValue.

]

{ #category : 'tests - isUsed' }
OCParserTest >> testIsUsedWhenInAssignmentExpectIsUsed [
	| tree |
	"an explicit return node is *used*"
	tree := self parserClass parseMethod:
			'newMethod
			| asdf |
	asdf := OrderedCollection new.
	asdf add: 1.
	^ asdf.'.
	self assert: tree body statements first value isUsedAsReturnValue
]

{ #category : 'tests - isUsed' }
OCParserTest >> testIsUsedWhenNonLocalReturnExpectNotUsed [
	| tree |
	" even a block node with non-local return is not *used* "
	tree := self parserClass parseMethod:
	      'tmp [^ 3+4 ]'.
	self deny: tree body statements last isUsedAsReturnValue.
]

{ #category : 'foreign use of test' }
OCParserTest >> testLiteralArray [
	"Test the good parsing of the elements of a literal array which means treating each as a literal."
	| tree collection |
	tree := self parserClass parseExpression:
			  '#( a #b #''c'' . + - 1 -2 3.4 #true true #false false #nil nil "comment" ''string'' #[ 1 2 3 ] #(1 2 3))'.
	collection := OrderedCollection new.
	"Create a collection with the expected values contained in the literal array."
	collection add: #a;
				  add: #b;
				  add: #c;
				  add: #'.';
				  add: #+;
				  add: #-;
				  add: 1;
				  add: -2;
				  add: 3.4;
				  add: #true;
				  add: true;
				  add: #false;
				  add: false;
				  add: #nil;
				  add: nil;
				  add: 'string';
				  add: #[1 2 3];
				  add: #(1 2 3).
	self assert: tree isLiteralArray.
	tree value with: collection do: [ :node :value |
		self assert: node value equals: value ]
]

{ #category : 'tests - Array' }
OCParserTest >> testLiteralArrayContainsLiteralValues [
	"Test the good parsing of the elements of a literal array which means treating each as a literal."
	| tree collection |
	tree := self parserClass parseExpression:
			  '#( a #b #''c'' . + - 1 -2 3.4 #true true #false false #nil nil "comment" ''string'' #[ 1 2 3 ] #(1 2 3))'.
	collection := OrderedCollection new.
	"Create a collection with the expected values contained in the literal array."
	collection add: #a;
				  add: #b;
				  add: #c;
				  add: #'.';
				  add: #+;
				  add: #-;
				  add: 1;
				  add: -2;
				  add: 3.4;
				  add: #true;
				  add: true;
				  add: #false;
				  add: false;
				  add: #nil;
				  add: nil;
				  add: 'string';
				  add: #[1 2 3];
				  add: #(1 2 3).
	self assert: tree isLiteralArray.
	tree value with: collection do: [ :node :value |
		self assert: node value equals: value ]
]

{ #category : 'tests - Literal' }
OCParserTest >> testLiteralIntevals [
	| tree |
	tree := self parserClass parseExpression: '#(#a b #( c ))'.
	self assert: tree contents first start equals: 3.
	self assert: tree contents first stop equals: 4.
	self assert: tree contents last contents first start equals: 11
]

{ #category : 'tests - matching' }
OCParserTest >> testMatchInContext [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree match: tree inContext: Dictionary new).

	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeForSelector: each.
			self assert: (tree match: tree inContext: Dictionary new) ] ]
]

{ #category : 'error testing' }
OCParserTest >> testMessageWithFaultySelectorIsFaulty [
	| tree |
	tree := self parseFaultyExpression: 'message 2'.
	self assert: tree isFaulty
]

{ #category : 'tests - Intervals' }
OCParserTest >> testMethodNodeEndAtTheEndOfTheString [
	| tree |
	tree := (self parserClass parseExpression: 'foo kung foo. 1+2+3. #''alabama''.  ').
	self assert: tree parent source equals: 'foo kung foo. 1+2+3. #''alabama''.  '.
	self assert: tree parent stop equals: tree parent sourceCode size.

	tree := (self parserClass parseMethod: 'foo kung foo. 1+2+3. #''alabama''.  ').
	self assert: tree source equals: 'foo kung foo. 1+2+3. #''alabama''.  '.
	self assert: tree stop equals: tree newSource size
]

{ #category : 'tests - Intervals' }
OCParserTest >> testMethodNodeStartAtOne [
	| tree |
	"When there are multiple statements, the node returned is the Sequence node (child of the method node)."
	tree := (self parserClass parseExpression: 'foo koong. ^foo').
	self assert: tree parent start equals: 1.

	"DoItMethod do not start at 1"
	tree := (self parserClass parseExpression: '  foo koong. ^foo').
	self assert: tree parent start equals: 3.

	"parseMethod always return a method node."
	tree := (self parserClass parseMethod: 'foo koong. ^foo').
	self assert: tree start equals: 1.

	tree := (self parserClass parseMethod: '  foo koong. ^foo').
	self assert: tree start equals: 1
]

{ #category : 'tests' }
OCParserTest >> testMethodPatterns [
	#(#('+ a ^self + a' #+) #('foo ^self foo' #foo) #('foo: a bar: b ^a + b' #foo:bar:))
		do: [ :each | self assert: (self parserClass parseMethodPattern: each first) identicalTo: each last ]
]

{ #category : 'tests' }
OCParserTest >> testModifying [
	| tree |
	tree := self parserClass parseMethod: 'foo: a bar: b | c | self first. self second. a + b + c'.
	self deny: tree lastIsReturn.
	self deny: (tree body statements at: 2) isUsedAsReturnValue.
	self assert: tree body statements last arguments first isUsedAsReturnValue.
	self assert: (tree isLast: tree body statements last).
	self deny: (tree isLast: tree body statements first).
	self assert: (tree defines: 'a').
	self deny: (tree defines: 'c').
	self assert: (tree body defines: 'c').
	self deny: (tree body defines: 'a').
	tree
		addReturn;
		selector: #bar:foo:.
	tree body
		addTemporaryNamed: 'd';
		removeTemporaryNamed: 'c'.
	self compare: tree to: (self parserClass parseMethod: 'bar: a foo: b | d | self first. self second. ^a + b + c').
	self
		assertEmpty:
			(tree argumentNames asSet
				removeAll: #('a' 'b');
				yourself).
	self
		assertEmpty:
			(tree allDefinedVariables asSet
				removeAll: #('a' 'b' 'd');
				yourself).
	tree := self parserClass parseExpression: 'self foo: 0'.
	tree selector: #+.
	self compare: tree to: (self parserClass parseExpression: 'self + 0')
]

{ #category : 'error testing' }
OCParserTest >> testNegativeNumberError [
	self should: [ self parserClass parseExpression: '- 2' ] raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testNodeWithMissingArgumentHasSameSourceAsOriginal [
	| tree source errorNode |
	source := '1+'.
	tree := self parseFaultyExpression: source.
	errorNode := tree arguments first.

	self assert: errorNode value isEmpty.
	self assert: errorNode sourceInterval isEmpty
]

{ #category : 'tests' }
OCParserTest >> testNodesDo [
	| size |
	size := 0.
	self treeWithEverything nodesDo: [ :e | size := size + 1 ].
	self assert: size equals: 19
]

{ #category : 'tests - Number' }
OCParserTest >> testNumberParsing [
	| numbers node |
	numbers := #(('1' 1) ('-1' -1) ('123' 123) ('123' 123) ('-123' -123) ('1.1' 1.1) ('-1.1' -1.1) ('1.23' 1.23) ('-1.23' -1.23) ('1e3' 1e3) ('1d3' 1d3) ('1q3' 1q3) ('-1e3' -1e3) ('1e-3' 1e-3) ('-1e-3' -1e-3) ('2r1e8'  2r1e8) ('-2r1e8' -2r1e8) ('2r1e-8' 2r1e-8) ('-2r1e-8' -2r1e-8) ('0.50s2' 0.50s2) ('0.500s3' 0.500s3) ('0.050s3' 0.050s3)).
	numbers do: [ :spec |
		node := self parserClass parseExpression: spec first.
		self assert: node source equals: spec first.
		self assert: node value equals: spec second ]
]

{ #category : 'tests - Number' }
OCParserTest >> testNumberRadixParsing [
	2 to: 32 do: [ :radix |
		| radixString |
		radixString := radix printString , 'r'.
		0 to: 72 do: [ :i | self assert: (self parserClass parseExpression: radixString , (i radix: radix)) value equals: i ] ]
]

{ #category : 'tests - Parentage' }
OCParserTest >> testParentOfArgumentNodeIsMethodNode [
	| tree |
	tree := self parseMethod: 'Selector: argument ^return something.'.
	self assert: tree arguments first isVariable.
	self assert: tree arguments first parent equals: tree
]

{ #category : 'tests - Parentage' }
OCParserTest >> testParentOfMethodNodeIsNil [
	| tree |
	tree := self parseMethod: 'Selector ^return something.'.
	self assert: tree parent isNil
]

{ #category : 'tests - Parentage' }
OCParserTest >> testParentOfSequenceNodeIsMethodNode [
	| tree |
	tree := self parseMethod: 'Selector ^return something.'.
	self assert: tree body isSequence.
	self assert: tree body parent equals: tree
]

{ #category : 'tests - Parentage' }
OCParserTest >> testParentOfStatementsIsSequenceNode [
	| tree |
	tree := self parseMethod: 'Selector: argument "comment node" | temporaries | message node. #literalNode . #(literal array node). {array node}. [block node]. cascade node; cascaded. assignement := node. ^return node'.
	(tree body statements) do: [ :each | self assert: each parent equals: tree body]
]

{ #category : 'tests' }
OCParserTest >> testParents [
	(Array with: self treeWithEverything with: self treeWithReallyEverything)
		do: [ :tree |
			(Array with: tree with: tree copy)
				do: [ :root |
					root
						nodesDo: [ :node |
							node children
								do: [ :each |
									(each parent isMessage and: [ each parent isCascaded ])
										ifFalse: [ self assert: each parent identicalTo: node.
											self assert: each methodNode identicalTo: root ] ] ] ] ]
]

{ #category : 'tests' }
OCParserTest >> testParseExpressionDontFreeze [

	"This test is here to chark that CI does not freeze or crash on code error."
	"The test sould just 'fail' gracefully."
	<expectedFailure>

	"Force a parse error (from a simple tool class).
	The image (and CI) should not freeze :)"
	self parserClass parseExpression: '#'
]

{ #category : 'tests - parsing' }
OCParserTest >> testParseExpressionGivesTheWriteAmountOfStatements [
	"Testing with a list of triplets.
	 1. The string parsed.
	 2. The number of statements expected in parsed string.
	 3. The number of dots marking a period expected in parsed string."
	| tree |
	#(
	('' 0 0)
	('.' 0 1)
	('| bar |' 0 0)
	('| bar | .' 0 1)
	('| bar | ..' 0 2)
	('foo. bar' 2 1)
	('foo. bar.' 2 2)
	('foo. bar. .' 2 3)
	('. foo. bar' 2 2)
	('foo. bar. foo bar.' 3 3)) do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size equals: each second.
		self assert: tree periods size equals: each last ]
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyLiteral [
	| faultyLiteral faultyLiteralArray node |
	"a literal or literal array object with an unknown character can be parse
	as faulty expression. If so, the error token (unknown character) needs to be consumed
	otherwise we get an endless loop"
	faultyLiteral := '¿'.
	faultyLiteralArray  := '#(¿)'.
	node := self parseFaultyExpression: faultyLiteral.
	self assert: node isParseError.
	self assert: node errorMessage equals: 'Unknown character'.
	node := self parseFaultyExpression: faultyLiteralArray.
	self assert: node isParseError.
	self assert: node contents first errorMessage equals: 'Unknown character'
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyMethod [
	| node strangeExpressions |

	strangeExpressions := OrderedCollection new .
	strangeExpressions
		add: 'method: asd   self ,';
		add: 'method: asd   self a:'; "message without needed argument"
		add: 'method: asd   []]';
		add: 'method: asd  [  ] ,';"binary message without second argument"
		add: 'method:  ^^'; "returning twice"
		add: 'method: asd  ['; "opening a block"
		add: 'method: asd  ^ {'; "Only Open a literal array"
		add: 'selector ''';
		add: 'selector ''part1''''part2'; "string with escaped ' does not end"
		add: 'selector #^';
		add: 'selector ¿';
		add: ':nl'.


	strangeExpressions do: [ :exp |
			node := self parseFaultyMethod: exp.
			self assert: node isMethod.
			self assert: node isFaulty
	]
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyMethodExpressionHasErrorNodeAsFinal [
	| node strangeExpression |

	strangeExpression := 'method: asd  ^ asd. n'.
	node := self parseFaultyMethod: strangeExpression.

	self assert: node isMethod.
	self deny: node isFaulty.
	self assert: node statements size equals: 2.
	self assert: (node statements last warningNotices last messageText) equals: 'Unreachable statement'
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyMethodMessagePattern [
	| node faultyMessagePattern |
	faultyMessagePattern := '1'.
	node := self parseFaultyMethod: faultyMessagePattern.
	self assert: node isFaulty
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyMethodStop [
	 | methodSource node |

	methodSource :=  'method: asd   self do      ('.
	node := self parseFaultyMethod: methodSource.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assert: node stop equals: 28
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseFaultyPragma [
	| node |
	node := self parseFaultyMethod: 'a <b:'.
	self assert: node isMethod.
	self assert: node pragmas size equals: 1.
	self assert: node pragmas first isFaulty
]

{ #category : 'tests - parsing' }
OCParserTest >> testParseGivesMethod [
	"The parsing of an expression or a method gives a method node.
	 For an expression we expect the selector to be #noMethod.
	 For a method we expect the selector to be the first identifier.
	 In the method node, we expect the original string (source) to be stored."
	| tree |
	"When there are multiple statements, the node returned is the Sequence node (child of the method node)."
	tree := (self parserClass parseExpression: 'foo koong. ^foo').
	self assert: tree parent isMethod.

	"When there is a single statement, the node returned is the overarching node of the statement (child of the sequence node)."
	tree := (self parserClass parseExpression: 'foo koong foo').
	self assert: tree parent parent isMethod.

	"parseMethod always return a method node."
	tree := (self parserClass parseMethod: 'foo koong. ^foo').
	self assert: tree isMethod.

	tree := (self parserClass parseMethod: 'foo koong foo').
	self assert: tree isMethod
]

{ #category : 'tests' }
OCParserTest >> testParseLiterals [

	self assert: (OCParser parseLiterals: '') equals: #(  ).
	self
		assert: (OCParser parseLiterals: '10 1.1 true nil false')
		equals: #( 10 1.1 true nil false ).
	self
		assert: (OCParser parseLiterals: '#foo #foo: #foo:bar:baz')
		equals: #( #foo #foo: #'foo:bar:baz' ).
	self
		assert: (OCParser parseLiterals: 'foo foo: foo:bar:baz')
		equals: #( #foo #foo: #'foo:bar:baz' ).
	self
		assert: (OCParser parseLiterals:
				 '''foo'' ''foo:'' ''foo:bar:baz'' ''1'' ''true''')
		equals: #( 'foo' 'foo:' 'foo:bar:baz' '1' 'true' ).
	self
		assert: (OCParser parseLiterals: '(1 2) #(1 2) #[1 2]')
		equals: #( #( 1 2 ) #( 1 2 ) #[ 1 2 ] ).
	self
		assert: (OCParser parseLiterals: '.[];::=^')
		equals: #( #'.' #'[' #']' #';' #':' #':=' #'^' ).

	self
		should: [ OCParser parseLiterals: '#( unclosed' ]
		raise: OCCodeError.
	self
		should: [ OCParser parseLiterals: ') unopened' ]
		raise: OCCodeError.
	self
		should: [ OCParser parseLiterals: '#[ 1 1000 2 ] bad subliteral' ]
		raise: OCCodeError.
	self
		should: [ OCParser parseLiterals: 'syntax¿error' ]
		raise: OCCodeError
]

{ #category : 'tests - Pattern' }
OCParserTest >> testParseMethodPatternGivesSelector [
	#(#('+ a ^self + a' 				#+)
	  #('foo ^self foo' 				#foo)
	  #('foo: a b ^self foo: b'    #foo:)
	  #('foo: a bar: b ^a + b' 		#foo:bar:))
		do: [ :each | self assert: (self parserClass parseMethodPattern: each first) identicalTo: each last ]
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testParseMethodWhichEndsWithUncompleteStringWithEscapedMarkAtTheEnd [
	| node strangeMethod body statement message errorNode |
	strangeMethod := '
	selector
		|temp|
		temp := ''this is right'', ''wrong'''''.
	node := self parseFaultyMethod: strangeMethod.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assertEmpty: node arguments.

	body := node body.
	self assert: body isSequence.
	self assert: body isFaulty.
	self assert: (body temporaries includes: (OCVariableNode named: 'temp')).

	statement := body statements first.
	self assert: statement isFaulty.
	self assert: statement isAssignment.

	message := statement value.
	self assert: message isFaulty.
	self assert: message arguments size equals: 1.

	errorNode := message arguments at: 1.
	self assert: errorNode isFaulty.
	self
		assert: errorNode value
		equals: '''wrong'''''.
	self assert: errorNode errorMessage equals: 'Unmatched '' in string literal.' translated
]

{ #category : 'tests - CreationProtocol' }
OCParserTest >> testParseMethodWithErrorTokenIsWellFormed [
	| node strangeMethod body statement message errorNode |
	strangeMethod := '
	selector
		|temp|
		temp := ''this is right'', ''wrong because no end.
		^temp'.
	node := self parseFaultyMethod: strangeMethod.

	self assert: node isMethod.
	self assert: node isFaulty.
	self assertEmpty: node arguments.

	body := node body.
	self assert: body isSequence.
	self assert: body isFaulty.
	self assert: (body temporaries includes: (OCVariableNode named: 'temp')).

	statement := body statements first.
	self assert: statement isFaulty.
	self assert: statement isAssignment.

	message := statement value.
	self assert: message isFaulty.
	self assert: message arguments size equals: 1.

	errorNode := message arguments at: 1.
	self assert: errorNode isFaulty.
	self
		assert: errorNode value
		equals:
			'''wrong because no end.
		^temp'.
	self assert: errorNode errorMessage equals: 'Unmatched '' in string literal.' translated
]

{ #category : 'tests - parsing' }
OCParserTest >> testParseRegistersTheSourceInMethodNode [
	"The parsing of an expression or a method gives a method node.
	 For an expression we expect the selector to be #noMethod.
	 For a method we expect the selector to be the first identifier.
	 In the method node, we expect the original string (source) to be stored."
	| tree |
	"When there are multiple statements, the node returned is the Sequence node (child of the method node)."
	tree := (self parserClass parseExpression: 'foo koong. ^foo').
	self assert: tree parent source equals: 'foo koong. ^foo'.

	"When there is a single statement, the node returned is the overarching node of the statement (child of the sequence node)."
	tree := (self parserClass parseExpression: 'foo koong foo').
	self assert: tree parent parent source equals: 'foo koong foo'.

	"parseMethod always return a method node."
	tree := (self parserClass parseMethod: 'foo koong. ^foo').
	self assert: tree source equals: 'foo koong. ^foo'.

	tree := (self parserClass parseMethod: 'foo koong foo').
	self assert: tree source equals: 'foo koong foo'
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParseUnfinishedBlockWithFaultyBody [
	| node unfinishedBlockExp missingArgExp unfinishedBlockAndMissingArgExp validBlock unfinishedBlockWithReturnExp|
	"Parsing a block as a faulty expression, where the block body is faulty and there is a missing closing bracket,
the whole expression is replaced by a single ASTParseErrorNode, this makes it difficult for the highlighter to
indicate the invalid expression."
	unfinishedBlockExp := '[:x | x missingArg: 3. '.
	missingArgExp := '[:x | x missingArg: . ]'.
	unfinishedBlockAndMissingArgExp := '[:x | x missingArg: . '.
	unfinishedBlockWithReturnExp := '[^ '.
	validBlock := '[:x | x missingArg: 3 ]. '.

	{unfinishedBlockExp. unfinishedBlockAndMissingArgExp. unfinishedBlockWithReturnExp}
		do: [ :faultyExpression |
			node := self parseFaultyExpression: faultyExpression.
			self assert: node isEnglobingError.
			self assert: node isBlockError ].

	node := self parseFaultyExpression: missingArgExp.
	self assert: node isBlock.
	self assert: node isFaulty.
	self assert: node body isFaulty.

	node := self parseFaultyExpression: validBlock.
	self assert: node isBlock.
	self deny: node isFaulty.
	self deny: node body isFaulty
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParserErrors [
	#(#('self foo. + 3' 11) #('self 0' 6) #('self asdf;;asfd' 11))
		do: [:each | self parseError: each]
]

{ #category : 'tests - parsing faulty' }
OCParserTest >> testParserErrorsWithErrorBlock [
	"Parse source with errors and ensure we identify them"

	#(#('self foo. + 3' 1) #('self 0' 1) #('self asdf;;asfd' 1))
		do: [:each | |ast errorCount|
			    ast := self parserClass new
					initializeParserWith: each first;
					parseExpression.

				errorCount := 0.
				OCParseErrorNodeVisitor visit: ast do: [ :n | errorCount := errorCount + 1 ].
				self assert: errorCount equals: each last ]
]

{ #category : 'tests - Array' }
OCParserTest >> testParsingArrayGivesArrayNode [
	| tree |
	#('{}' '{foo}' '{foo. bar.}')
	 do: [ :each |
		tree := self parserClass parseExpression: each .
		self assert: tree isDynamicArray.]
]

{ #category : 'tests - parsing' }
OCParserTest >> testParsingLiteralMessages [
	self assert: (self parserClass parseExpression: 'nil self nil') isMessage.
	self assert: (self parserClass parseExpression: 'self true') isMessage.
	self assert: (self parserClass parseExpression: 'self false') isMessage.
	self assert: (self parserClass parseExpression: 'self -1') isMessage.

	self assert: (self parserClass parseMethod: 'nil') isMethod.
	self assert: (self parserClass parseMethod: 'true') isMethod.
	self assert: (self parserClass parseMethod: 'false') isMethod
]

{ #category : 'tests - Pragma' }
OCParserTest >> testParsingPragmaRecogniseAndStorePragmas [
	"Tests that the pragmas are correctly interpreted.
	 1. The string to be parsed.
	 2. The selector in parsed pragma.
	 3. The selector arguments."
	| tree |
	#('foo <foo>'
	'foo <foo: 1>'
	'foo <foo: 1.2>'
	'foo <foo: -3>'
	'foo <foo: #a>'
	'foo <foo: #< >'
	'foo <foo: #> >'
	'foo <foo: $a>'
	'foo <foo: ''bar''>'
	'foo <foo: true>'
	'foo <foo: false>'
	'foo <foo: nil>'
	'foo <foo: #(1 2)>'
	'foo <foo: 1 bar: 2>'
	'foo |t| <foo: 1.2>'
	'foo |t| <foo: 1 bar: 2>' ) do: [ :each |
		tree := self parserClass parseMethod: each.
		self assert: tree pragmas size equals: 1.].

	self assert: (self parserClass parseMethod: 'foo |t| <pragma1> <pragma2>') pragmas size equals: 2
]

{ #category : 'tests - Pragma' }
OCParserTest >> testParsingPragmaRecogniseArguments [
	"Tests that the pragmas are correctly interpreted.
	 1. The string to be parsed.
	 2. The selector in parsed pragma.
	 3. The selector arguments."
	| tree node |
	#(('foo <foo>' 				#foo ())
	('foo <foo: 1>' 				#foo: (1))
	('foo <foo: 1.2>' 			#foo: (1.2))
	('foo <foo: -3>' 			#foo: (-3))
	('foo <foo: #a>' 			#foo: (a))
	('foo <foo: #< >' 			#foo: (<))
	('foo <foo: #> >' 			#foo: (>))
	('foo <foo: $a>' 			#foo: ($a))
	('foo <foo: ''bar''>' 		#foo: ('bar'))
	('foo <foo: true>' 			#foo: (true))
	('foo <foo: false>' 			#foo: (false))
	('foo <foo: nil>' 			#foo: (nil))
	('foo <foo: #(1 2)>' 		#foo: ((1 2)))
	('foo <foo: 1 bar: 2>' 		#foo:bar: (1 2))
	('foo |t| <foo: 1.2>' 		#foo: (1.2))
	('foo |t| <foo: 1 bar: 2>' #foo:bar: (1 2))
	) do: [ :each |
		tree := self parserClass parseMethod: each first.
		self assert: tree pragmas size equals: 1.
		node := tree pragmas first.
		1 to: node arguments size do: [ :i |
			self assert: (node arguments at: i) value equals: (each last at: i) ].]
]

{ #category : 'tests - Pragma' }
OCParserTest >> testParsingPragmaRecogniseSelector [
	"Tests that the pragmas are correctly interpreted.
	 1. The string to be parsed.
	 2. The selector in parsed pragma.
	 3. The selector arguments."
	| tree node |
	#(('foo <foo>' 				#foo ())
	('foo <foo: 1>' 				#foo: (1))
	('foo <foo: 1.2>' 			#foo: (1.2))
	('foo <foo: -3>' 			#foo: (-3))
	('foo <foo: #a>' 			#foo: (a))
	('foo <foo: #< >' 			#foo: (<))
	('foo <foo: #> >' 			#foo: (>))
	('foo <foo: $a>' 			#foo: ($a))
	('foo <foo: ''bar''>' 		#foo: ('bar'))
	('foo <foo: true>' 			#foo: (true))
	('foo <foo: false>' 			#foo: (false))
	('foo <foo: nil>' 			#foo: (nil))
	('foo <foo: #(1 2)>' 		#foo: ((1 2)))
	('foo <foo: 1 bar: 2>' 		#foo:bar: (1 2))
	('foo |t| <foo: 1.2>' 		#foo: (1.2))
	('foo |t| <foo: 1 bar: 2>' #foo:bar: (1 2))
	) do: [ :each |
		tree := self parserClass parseMethod: each first.
		node := tree pragmas first.
		self assert: node selector equals: each second.]
]

{ #category : 'tests - Interval' }
OCParserTest >> testPositions [
	| blockNode |
	blockNode := self parserClass parseExpression: '[:a :b | ]'.
	self assert: blockNode left equals: 1.
	self assert: blockNode right equals: 10.
	self assert: blockNode bar equals: 8.
	self assert: blockNode sourceInterval equals: (1 to: 10).
	self assert: blockNode size equals: 1.	"test dummy collection protocol"
	blockNode printString.	"coverage"
	self deny: (blockNode isLast: (OCVariableNode named: 'b'))
]

{ #category : 'error testing' }
OCParserTest >> testPragmaConstantLiteralArgument [
	| primitiveDeclartion validPragma invalidPragma |
	"primitive declarations with variable (code) are allowed"
	primitiveDeclartion := 'foo <primitive: #dooo module: #bar error: code> ^ nil'.
	"#Point as argument is allowed"
	validPragma := 'foo <return: #Point> ^ 0@0'.
	"Point as argument is not allowed"
	invalidPragma := 'foo <return: Point> ^ 0@0'.
	self shouldnt:[self parserClass parseMethod: primitiveDeclartion] raise: OCCodeError.
	self shouldnt:[self parserClass 	parseMethod: validPragma] raise: OCCodeError.
	self should:[self parserClass parseMethod: invalidPragma] raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testPragmaImplicitLiteralArrayIsInvalid [
	| pragmaWithExpressionAsArgument pragmaWithLiteralArrayAsArgument pragmaWithStringAsArgument |
	"a pragma with arguments that look like code
	(3+4) were implicitly converted to a literal array
	#(3+4), this is not allowed anymore"
	pragmaWithExpressionAsArgument := 'foo
	<func: (3+4) res: 7>
	^ self'.
	self should:[self parserClass parseMethod: pragmaWithExpressionAsArgument] raise: OCCodeError.

	"an explicit literal array is allowed"
	pragmaWithLiteralArrayAsArgument := 'foo
	<func: #(3+4) res: 7>
	^ self'.
	"Should work"
	self shouldnt: [self parserClass parseMethod: pragmaWithLiteralArrayAsArgument] raise: OCCodeError.

	"and of course a string literal"
	pragmaWithStringAsArgument := 'foo
	<func: ''(3+4)'' res: 7>
	^ self'.
	"should work"
	self shouldnt: [self parserClass parseMethod: pragmaWithStringAsArgument] raise: OCCodeError
]

{ #category : 'tests' }
OCParserTest >> testPragmaInExpression [
	"Test if a working pragma in a method is faulty in an expression."
	| node |
	node := self parserClass parseExpression: '| token | <some: #tag> true'.
	self assert: (node methodNode hasPragmaNamed: #some:).

	node := self parserClass parseExpression: '<some: #tag> true'.
	self assert: (node methodNode hasPragmaNamed: #some:).

	self should: [(self parserClass parseExpression: '| token | 1+2. <some: #tag> true')]
		  raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testPragmaPrimitiveError [
	| tree  pragma |
	tree := self parserClass
				parseMethod: 'veryBasicAt: index
	<primitive: #dooo module: #bar error: code>
	^ code'.

	self assert: tree pragmas size equals: 1.

	pragma := tree pragmas first.
	self assert: pragma selector equals: #primitive:module:error:
]

{ #category : 'tests - parsing' }
OCParserTest >> testPragmas [
	| tree node |
	#(('foo <foo>' 				#foo ())
	('foo <foo: 1>' 				#foo: (1))
	('foo <foo: 1.2>' 			#foo: (1.2))
	('foo <foo: -3>' 			#foo: (-3))
	('foo <foo: #a>' 			#foo: (a))
	('foo <foo: #< >' 			#foo: (<))
	('foo <foo: #> >' 			#foo: (>))
	('foo <foo: $a>' 			#foo: ($a))
	('foo <foo: ''bar''>' 		#foo: ('bar'))
	('foo <foo: true>' 			#foo: (true))
	('foo <foo: false>' 			#foo: (false))
	('foo <foo: nil>' 			#foo: (nil))
	('foo <foo: #(1 2)>' 		#foo: ((1 2)))
	('foo <foo: 1 bar: 2>' 		#foo:bar: (1 2))
	('foo |t| <foo: 1.2>' 		#foo: (1.2))
	('foo |t| <foo: 1 bar: 2>' #foo:bar: (1 2))
	) do: [ :each |
		tree := self parserClass parseMethod: each first.
		self assert: tree pragmas size equals: 1.
		node := tree pragmas first.
		self assert: node selector equals: each second.
		1 to: node arguments size do: [ :i |
			self assert: (node arguments at: i) value equals: (each last at: i) ].
		self assert: (each first at: node start) equals: $<.
		self assert: (each first at: node stop) equals: $> ].

	self assert: (self parserClass parseMethod: 'selector <a>') isMethod.
	self assert: (self parserClass parseMethod: '
	selector
		"comment1"
		|temp|
		"comment2"
		<a>
		#<.
		^1') isMethod.
	self should: [self parserClass parseMethod: '
	selector
		"comment1"
		|temp|
		"comment2"
		<a>
		<.
		^1'] raise: OCCodeError
]

{ #category : 'tests - parsing' }
OCParserTest >> testPrimitives [
	self assert: (Object parseTreeForSelector: #basicAt:) isPrimitive.
	#(('foo ^true' false ) ('foo <some: #tag> ^true' false ) (' foo <some: #tag> <primitive: 123> ^true' true ) )
			do: [:each | self assert: (self parserClass parseMethod: each first) isPrimitive equals: each last]
]

{ #category : 'tests' }
OCParserTest >> testQuerying [
	| tree aNode arg1Node bNode |
	tree := self parserClass
		parseMethod:
			('test: a`	| b |`	b := (self foo: a; bar) baz.`	b := super test: b.`	^[:arg1 | self foa1 + (super foo: arg1 foo: a foo: b)]'
				copyReplaceAll: '`'
				with: (String with: (Character value: 13))).
	tree doSemanticAnalysis.
	self assert: (tree selfMessages collect: [ :each | each value ]) asSortedCollection asArray equals: #(#bar #foa1 #foo:).
	self assert: (tree superMessages collect: [ :each | each value ]) asSortedCollection asArray equals: #(#foo:foo:foo: #test:).
	aNode := tree whichNodeIsContainedBy: (112 to: 112).
	self assert: aNode name equals: 'a'.
	bNode := tree whichNodeIsContainedBy: (119 to: 119).
	self assert: bNode name equals: 'b'.
	arg1Node := tree whichNodeIsContainedBy: (102 to: 105).
	self assert: arg1Node name equals: 'arg1'.
	self assert: (arg1Node statementNode isMessage and: [ arg1Node statementNode selector value = #+ ]).
	self assert: (arg1Node statementNodeIn: arg1Node methodOrBlockNode) equals: arg1Node statementNode.
	self assert: (arg1Node statementNodeIn: arg1Node methodOrBlockNode body) equals: arg1Node statementNode.
	self assert: (arg1Node statementNodeIn: tree) equals: tree body statements last.
	self assert: (arg1Node statementNodeIn: tree body) equals: tree body statements last.
	self should: [ arg1Node statementNodeIn: OCSequenceNode new ] raise: NotFound.
	self assert: (arg1Node whoDefines: 'arg1') isBlock.
	self assert: (aNode whoDefines: 'a') isMethod.
	self assert: (aNode whoDefines: 'b') isSequence.
	self assert: (tree whichNodeIsContainedBy: (91 to: 119)) selector value equals: #foo:foo:foo:.
	self assert: (tree whichNodeIsContainedBy: (69 to: 121)) isBlock.
	self assert: (tree whichNodeIsContainedBy: (69 to: 118)) isNil.
	self assert: aNode blockVariables asSortedCollection asArray equals: #('arg1').
	self assert: aNode temporaryVariables asSortedCollection asArray equals: #('b').
	self assert: tree allDefinedVariables asSortedCollection asArray equals: #('a' 'arg1' 'b').
	self assert: tree allArgumentVariables asSortedCollection asArray equals: #('a' 'arg1').
	self assert: tree allTemporaryVariables asSortedCollection asArray equals: #('b')
]

{ #category : 'to be reworked' }
OCParserTest >> testQueryingPrimitiveErrorVar [
	| tree |
	tree := self parserClass
				parseMethod: 'clone
	"Answer a shallow copy of the receiver."
	<primitive: 148 error: ec>
	| copy |
	ec == #''insufficient object memory'' ifFalse:
		[|nothing| nothing := nil.
		^ noting].
   copy := nil.
	^ copy'.
	self assert: (tree body whoDefines:'copy') equals: tree body.
	self assert: (tree blockNodes first body whoDefines: 'nothing') equals: tree blockNodes first body.
	self assert: (tree body whoDefines: 'ec') equals: tree
]

{ #category : 'tests' }
OCParserTest >> testReplacingNodes [
	| tree search block |
	tree := self parserClass parseMethod: '+ a | a b | self ifTrue: [a] ifFalse: [b := c]. a := b. [:b :c :a | a foo: a; foo1: a; foo2: a foo: b]. {a. b}. ^a'.
	search := self parseTreeSearcher.
	block := [ :aNode :answer | aNode replaceWith: (OCVariableNode named: 'q') ].
	search
		matches: 'a' do: block;
		matchesArgument: 'a' do: block.
	search executeTree: tree.
	self
		assert: tree
		equals:
			(self parserClass parseMethod: '+ q | q b | self ifTrue: [q] ifFalse: [b := c]. q := b. [:b :c :q | q foo: q; foo1: q; foo2: q foo: b]. {q. b}. ^q').
	self assert: tree removeDeadCode equals: (self parserClass parseMethod: '+ q | q b | self ifTrue: [] ifFalse: [b := c]. q := b. {q. b}. ^q')
]

{ #category : 'tests' }
OCParserTest >> testScopesEnclosingMatchingLiterals [
	| tree |

	tree := self parserClass parseMethod: 'tmp [ $] ]'.
	self assert: tree statements first isBlock.

	tree := self parserClass parseMethod: 'tmp ($))'.
	self assert: tree statements first isLiteralNode.

	tree := self parserClass parseMethod: 'tmp #( $) )'.
	self assert: tree statements first isLiteralArray.

	tree := self parserClass parseMethod: 'tmp { $} }'.
	self assert: tree statements first isDynamicArray
]

{ #category : 'tests - Selector' }
OCParserTest >> testSelectorFromMessageIsSelectorNode [
	| tree |
	tree := self parseExpression: 'receiver selector'.
	self assert: tree isMessage.
	self assert: tree selectorNode isSelector.
	self assert: tree selector equals: #selector
]

{ #category : 'tests - Cascade' }
OCParserTest >> testSemiColonAfterMessageIsCascade [
	self assert: (self parserClass parseExpression: ' self msg; yourself') isCascade.
	self assert: (self parserClass parseExpression: ' self msg:(arg msg:arg); yourself') isCascade.
	self assert: (self parserClass parseExpression: ' self msg: arg msga:arga ; ibm ; yourself') isCascade
]

{ #category : 'tests - Intervals' }
OCParserTest >> testSequenceNodeEndAtTheEndOfTheString [
	| tree |

	tree := (self parserClass parseExpression: 'foo kung foo. 1+2+3. #''alabama''.').
	self assert: tree source equals: 'foo kung foo. 1+2+3. #''alabama''.'.
	self assert: tree stop equals: tree parent source size.

	tree := (self parserClass parseMethod: 'foo kung foo. 1+2+3. #''alabama''.').
	self assert: tree source equals: 'foo kung foo. 1+2+3. #''alabama''.'.
	self assert: tree body stop equals: tree source size
]

{ #category : 'tests - Intervals' }
OCParserTest >> testSequenceNodeStartAtFirstTokenAfterTheSelector [
	"Beginning spaces are trimmed before the start of sequence node."
	| tree |
	"Here, the selector is the implicit #noMethod"
	tree := (self parserClass parseExpression: 'foo koong. ^foo').
	self assert: tree start equals: 1.

	"There are 2 spaces before foo."
	tree := (self parserClass parseExpression: '  foo koong. ^foo').
	self assert: tree start equals: 3.

	"Here, foo is the method selector so the sequence start at koong."
	tree := (self parserClass parseMethod: 'foo koong. ^foo').
	self assert: tree body start equals: 5.

	"There are 2 spaces before foo."
	tree := (self parserClass parseMethod: '  foo koong. ^foo').
	self assert: tree body start equals: 7
]

{ #category : 'tests - parsing' }
OCParserTest >> testStatements [
	| tree |
	#(('' 0 0) ('.' 0 1) ('| bar |' 0 0) ('| bar | .' 0 1) ('| bar | ..' 0 2) ('foo. bar' 2 1) ('foo. bar.' 2 2) ('foo. bar. .' 2 3) ('. foo. bar' 2 2)) do: [ :each |
		tree := self parserClass parseExpression: each first.
		self assert: tree statements size equals: each second.
		self assert: tree periods size equals: each last ]
]

{ #category : 'foreign use of test' }
OCParserTest >> testString [
	self assert: (self parserClass parseMethod: 'selector ^ ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<'', self') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector '':=''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''<''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ''>''') isMethod.
	self assert: (self parserClass parseMethod: 'selector ^ ''<>''') isMethod
]

{ #category : 'tests - parsing' }
OCParserTest >> testSymbolLiteral [
	| tree |
	#(
	'#<' #<
	'#>' #>
	'#<>' #<>
	'##foo' #foo
	'###foo' #foo
	'#foo:' #foo:
	'#foo::' #'foo::'
	'#foo::bar' #'foo::bar'
	'#foo::bar:' #'foo::bar:'
	'#foo::bar::' #'foo::bar::') pairsDo: [ :parseString :expectedResult |
		tree := self parserClass parseExpression: parseString.
		self assert: tree value equals: expectedResult.
		self assert: tree start equals: 1.
		self assert: tree stop equals: parseString size ].

	#(
	('#"bar"foo' #foo)
	('# foo' #foo)
	('#1' 1)
	('#12' 12)
	('#12.3' 12.3)
	('# 1' 1)
	('##1' 1)
	('#"bar"1' 1)) do: [ :pair |
		self should: [self parserClass parseExpression: pair first] raise: OCCodeError ]
]

{ #category : 'tests - Literal' }
OCParserTest >> testSymbolLiteralSuccessfullyCreateLiteralNode [
	"Test if the parsing of the strings gives a literal node corresponding to its' pair."
	| tree |
	#(
	'#<' #<
	'#>' #>
	'#<>' #<>
	'##foo' #foo
	'###foo' #foo
	'#foo:' #foo:
	'#foo::' #'foo::'
	'#foo::bar' #'foo::bar'
	'#foo::bar:' #'foo::bar:'
	'#foo::bar::' #'foo::bar::') pairsDo: [ :parseString :expectedResult |
		tree := self parserClass parseExpression: parseString.
		self assert: tree isLiteralNode.
		self assert: tree value equals: expectedResult.
		self assert: tree start equals: 1.
		self assert: tree stop equals: parseString size ]
]

{ #category : 'error testing' }
OCParserTest >> testUnclosedArrayErrorNodeContainsRightValue [
	| tree |
	tree := self parseFaultyExpression: '{ faulty parentheses message node'.
	self assert: tree isParseError.
	self assert: tree formattedCode equals: '{ faulty parentheses message node'
]

{ #category : 'error testing' }
OCParserTest >> testUnclosedBracketErrorNodeContainsRightValue [
	| tree |
	tree := self parseFaultyExpression: '[ faulty parentheses message node'.
	self assert: tree isEnglobingError.
	self assert: tree formattedCode equals: '[ faulty parentheses message node'
]

{ #category : 'error testing' }
OCParserTest >> testUnclosedParenthesesErrorNodeContainsRightValue [
	| tree |
	tree := self parseFaultyExpression: '( faulty parentheses message node'.
	self assert: tree isEnglobingError.
	self assert: tree formattedCode equals: '( faulty parentheses message node'
]

{ #category : 'garbage tests' }
OCParserTest >> testUnclosedParenthesesRaiseSyntaxError [
	"Should propose a closing parenthese before the end of the statement. Should also offer possibility to quickly erase it."
	self should: [self parserClass parseExpression: '(temp := 1 + 3 .'] raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testUnclosedTemporariesErrorNodeContainsRightValue [
	| tree |
	self skip. 
	"This test has an extra space on the server but not locally."
	tree := self parseFaultyExpression: '| faulty temporaries'.
	self assert: tree isEnglobingError.
	self assert: tree formattedCode  equals: '| faulty temporaries |', String cr
]

{ #category : 'error testing' }
OCParserTest >> testUnfinishedStatementWithLeadingBracesRaisesError [

	self should: [(self parserClass parseExpression: '[]}')]
		  raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testUnfinishedStatementWithLeadingBracketRaisesError [

	self should: [(self parserClass parseExpression: '[]]')]
		  raise: OCCodeError
]

{ #category : 'error testing' }
OCParserTest >> testUnfinishedStatementWithLeadingParenthesisRaisesError [

	self should: [(self parserClass parseExpression: '[])')]
		  raise: OCCodeError
]

{ #category : 'private' }
OCParserTest >> treeWithEverything [
	^ self parserClass
		parseMethod:
			'method: arg1 | temps | temps := #(10). temps foo; foo. ^(temps collect: [:e | ])'
]

{ #category : 'private' }
OCParserTest >> treeWithReallyEverything [
	^ self parserClass
		parseMethod:
			'method: arg1 <foo> <foo: 1.2> | temps | temps := #[ 1 2 3 ]. temps := #(true false nil 1 1.2 $a foo #foo ''foo'' #() #(1 2) #[] #[1 2]). { 1 negated. 1 + 2. 1 raisedTo: 2 }. temps foo; foo: self. ^ (temps collect: [:e | | btemps | ((e isNil)) ])'
]
