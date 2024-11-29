"
RBParser takes a source code string and generates an AST for it. This is a hand-written, recursive descent parser and has been optimized for speed. The simplest way to call this is either 'RBParser parseExpression: aString' if you want the AST for an expression, or 'RBParser parseMethod: aString' if you want to parse an entire method.

Instance Variables:
	currentToken	<ASTToken>	The current token being processed.
	emptyStatements	<Boolean>	True if empty statements are allowed. In IBM, they are, in VW they aren't.
	errorBlock	<BlockClosure>	The block to evaluate on a syntax error.
	nextToken	<ASTToken>	The next token that will be processed. This allows one-token lookahead.
	scanner	<RBScanner>	The scanner that generates a stream of tokens to parse.
	source	<String>	The source code to parse
	tags	<Collection of: Interval>	The source intervals of the tags appearing at the top of a method (e.g. Primitive calls)

Shared Variables:
	ParserType	<Symbol>	the type code we are parsing
"
Class {
	#name : 'RBParser',
	#superclass : 'Object',
	#instVars : [
		'scanner',
		'currentToken',
		'nextToken',
		'source',
		'comments',
		'pragmas'
	],
	#category : 'AST-Core-Parser',
	#package : 'AST-Core',
	#tag : 'Parser'
}

{ #category : 'parsing' }
RBParser class >> parseExpression: aString [
	^self parseExpression: aString onError: nil
]

{ #category : 'parsing' }
RBParser class >> parseExpression: aString onError: aBlock [
	^ (self parseFaultyExpression: aString) checkFaulty: aBlock
]

{ #category : 'parsing' }
RBParser class >> parseFaultyExpression: aString [
	"parse aString even if syntactically incorrect. Instead of raising an error, we create an AST with ASTParseErrorNode"
	^ self new
		initializeParserWith: aString;
		parseExpression
]

{ #category : 'parsing' }
RBParser class >> parseFaultyMethod: aString [
	"parse aString even if syntactically incorrect. Instead of raising an error, we create an AST with ASTarseErrorNode"
	^ self new
		initializeParserWith: aString;
		parseMethod
]

{ #category : 'parsing' }
RBParser class >> parseLiterals: aString [

	| scanner parser |
	parser := self new.
	scanner := parser scannerClass on: (ReadStream on: aString).
	scanner state: #literalArray.
	^ parser
		  source: aString;
		  scanner: scanner;
		  parseLiterals
]

{ #category : 'parsing' }
RBParser class >> parseMethod: aString [
	^self parseMethod: aString onError: nil
]

{ #category : 'parsing' }
RBParser class >> parseMethod: aString onError: aBlock [
	^ (self parseFaultyMethod: aString) checkFaulty: aBlock
]

{ #category : 'parsing' }
RBParser class >> parseMethodPattern: aString [
	^ (self new
		   initializeParserWith: aString;
		   parseMessagePattern) selector
]

{ #category : 'parsing' }
RBParser class >> parsePragma: aString [
	^ self new
		  initializeParserWith: aString;
		  parsePragmaAndReturn
]

{ #category : 'parsing' }
RBParser class >> parseRewriteExpression: aString [
	^self parseRewriteExpression: aString onError: nil
]

{ #category : 'parsing' }
RBParser class >> parseRewriteExpression: aString onError: aBlock [
	^RBPatternParser parseExpression: aString onError: aBlock
]

{ #category : 'parsing' }
RBParser class >> parseRewriteMethod: aString [
	^self parseRewriteMethod: aString onError: nil
]

{ #category : 'parsing' }
RBParser class >> parseRewriteMethod: aString onError: aBlock [
	^RBPatternParser parseMethod: aString onError: aBlock
]

{ #category : 'private' }
RBParser >> addCommentsTo: aNode [
	aNode comments: aNode comments , comments.
	comments do: [ :each | each parent: aNode ].
	comments := OrderedCollection new
]

{ #category : 'private - parsing' }
RBParser >> addParserError: errorMessage to: aNode [
	| errorNode |
	errorNode := self parseErrorNode: errorMessage.
	aNode addFaultyNode: errorNode.
	^ aNode
]

{ #category : 'private' }
RBParser >> addPragma: aPragma [
	pragmas ifNil: [ pragmas := OrderedCollection new ].
	pragmas addLast: aPragma
]

{ #category : 'private - classes' }
RBParser >> arrayNodeClass [
	^ ASTArrayNode
]

{ #category : 'private - classes' }
RBParser >> assignmentNodeClass [
	^ ASTAssignmentNode
]

{ #category : 'testing' }
RBParser >> atEnd [
	^currentToken isEOF
]

{ #category : 'private - parsing' }
RBParser >> basicParsePragma [
	"Current token must be Identifier, Keyword or Binary. Else it throws an error."
	^ currentToken isIdentifier
		ifTrue: [ self parseUnaryPragma ]
		ifFalse: [
			currentToken isKeyword
				ifTrue: [
					currentToken isPrimitiveKeyword
						ifTrue: [ ^ self parsePrimitiveKeywordPragma ].
					self parseKeywordPragma ]
				ifFalse: [ self parseBinaryPragma ] ]
]

{ #category : 'private - classes' }
RBParser >> blockNodeClass [
	^ ASTBlockNode
]

{ #category : 'private - classes' }
RBParser >> cascadeNodeClass [
	^ ASTCascadeNode
]

{ #category : 'accessing' }
RBParser >> currentToken [
	^currentToken
]

{ #category : 'private - classes' }
RBParser >> doItMethodNodeClass [
	^ ASTDoItMethodNode
]

{ #category : 'private - classes' }
RBParser >> englobingErrorNodeClass [
	^ASTEnglobingErrorNode
]

{ #category : 'error handling' }
RBParser >> errorBlock [

	self deprecated: 'see errorBlock:'
]

{ #category : 'accessing' }
RBParser >> errorBlock: aBlock [

	self deprecated: 'RBParser instances offer no more errorBlock facility. Use helpers methods on the class side, or `checkFaulty: aBlock` on a parsed AST'
]

{ #category : 'error handling' }
RBParser >> errorPosition [
	^currentToken ifNotNil: [ :token | token start] ifNil:[1]
]

{ #category : 'private' }
RBParser >> extractCommentsFrom: aToken [

	aToken comments
		collect: [:commentToken | ASTCommentNode with: commentToken ]
		into: comments
]

{ #category : 'error handling' }
RBParser >> getErrorFromClosuresWithMissingOpenings: aCollection [
	^((currentToken isSpecial and: ['])}' includes: currentToken value])
				" We make a distinction between the bracket and the other closures because blocks are the only ones that can contain multiple statements. "
				ifTrue: [ currentToken value = $)
					ifFalse: [ | errorNode |
								  errorNode := self parseEnglobingError: aCollection copy with: currentToken errorMessage: (currentToken value = $] ifTrue: [ '''['' expected' ]ifFalse: [ '''{'' expected' ]).
								  aCollection removeAll. errorNode ]
					ifTrue: [  aCollection isEmpty
							ifFalse: [| errorNode | errorNode := self parseEnglobingError: (OrderedCollection with: aCollection last) with: currentToken errorMessage: ''')'' expected'.  aCollection removeLast. errorNode]
							ifTrue: [ self parseEnglobingError: aCollection copy with: currentToken errorMessage: ''')'' expected'] ] ]
				ifFalse: [ self parseErrorNode: 'Unknown input at end' ])
]

{ #category : 'initialization' }
RBParser >> initialize [

	super initialize.
	comments := OrderedCollection new
]

{ #category : 'accessing' }
RBParser >> initializeParserWith: aString [
	source := aString.
	self scanner: (self scannerClass on: (ReadStream on: aString))
]

{ #category : 'private - classes' }
RBParser >> literalArrayNodeClass [
	^ ASTLiteralArrayNode
]

{ #category : 'private - classes' }
RBParser >> literalNodeClass [
	^ ASTLiteralNode
]

{ #category : 'private - classes' }
RBParser >> literalValueNodeClass [
	^ ASTLiteralValueNode
]

{ #category : 'private - classes' }
RBParser >> messageNodeClass [
	^ ASTMessageNode
]

{ #category : 'private - classes' }
RBParser >> methodNodeClass [
	^ ASTMethodNode
]

{ #category : 'private' }
RBParser >> nextToken [
	^nextToken ifNil: [nextToken := self scannerNext]
]

{ #category : 'private - parsing' }
RBParser >> parseArray [
	"Although an array node represents an array, it is not an Array-like object."
	| startToken node |
	startToken := currentToken.
	self step.
	node := self arrayNodeClass new.
	node left: startToken start.
	self parseStatementList: false  into: node untilAnyCloserOf: '}'.
	(currentToken isSpecial: $})
		ifFalse: [ ^ self parseEnglobingError: node statements with: startToken errorMessage: '''}'' expected'].
	node right: currentToken start.
	self step.
	^ node
]

{ #category : 'private - parsing' }
RBParser >> parseAssignment [
	"Need one token lookahead to see if we have a ':='. This method could
	make it possible to assign the literals true, false and nil."

	| node position err |
	currentToken isAssignment ifTrue: [
		err := self parseErrorNode: 'variable expected in assigment'.
		self step. "Consume the :="
		node := self parseAssignment. "parse the right side"
		node := ASTAssignmentErrorNode from: err contents: { node }.
		node value: ':='.
		^ node
	].

	(currentToken isIdentifier and: [self nextToken isAssignment])
		ifFalse: [^self parseCascadeMessage].

	node := self parseVariableNode.
	position := currentToken start.
	self step.
	^self assignmentNodeClass
		variable: node
		value: self parseAssignment
		position: position
]

{ #category : 'private - parsing' }
RBParser >> parseBinaryMessage [
	| node |
	node := self parseUnaryMessage.

	[currentToken isLiteralToken ifTrue: [self patchNegativeLiteral].
	currentToken isBinary]
			whileTrue: [node := self parseBinaryMessageWith: node].
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseBinaryMessageWith: aNode [
	| binaryNode |
	binaryNode := self selectorNodeClass
		value: currentToken value
		keywordPositions: { currentToken start }.
	self step.
	^self messageNodeClass
		receiver: aNode
		selector:  binaryNode
		keywordsPositions: binaryNode keywordPositions
		arguments: { self parseUnaryMessage }
]

{ #category : 'private - parsing' }
RBParser >> parseBinaryPattern [
	| binaryToken node args |
	currentToken isBinary
		ifFalse: [
		^ self methodNodeClass
			selector: #''
			arguments: #()
			body: (self sequenceNodeClass statements: (OrderedCollection with: (self parseErrorNode: 'Message pattern expected')))].

	binaryToken := currentToken.
	self step.
	args := Array with: self parseVariableNode.
	node := self methodNodeClass
		selector: binaryToken value asSymbol
		keywordsPositions: (Array with: binaryToken start)
		arguments: args.
	node comments: node comments , args last comments.
	args last comments ifNotEmpty: [ args last comments: #() ].
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseBinaryPragma [
	| binaryToken |
	currentToken isBinary
		ifFalse: [ ^ self parseErrorNode: 'Message pattern expected' ].
	binaryToken := currentToken.
	self step.
	^ self pragmaNodeClass
		selector: binaryToken value asSymbol
		keywordsPositions: (Array with: binaryToken start)
		arguments: (Array with: self parsePragmaLiteral)
]

{ #category : 'private - parsing' }
RBParser >> parseBlock [
	| startToken node |
	startToken := currentToken.
	self step.
	node := self blockNodeClass new.
	self parseBlockArgsInto: node.
	self addCommentsTo: node.
	node left: startToken start.
	node body: self sequenceNodeClass new.

	self
		parseStatements: false
		into: node body
		untilAnyCloserOf: ']'.

	(currentToken isSpecial: $])
		ifFalse: [
			^ (self parseEnglobingError: { node body } with: startToken errorMessage: ''']'' expected')
				arguments: node arguments;
				stop: currentToken start - 1;
				yourself
		].
	node right: currentToken start.
	self step.
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseBlockArgsInto: node [
	| verticalBar args arg colons |
	args := OrderedCollection new: 2.
	colons := OrderedCollection new: 2.
	verticalBar := false.
	[currentToken isSpecial: $:] whileTrue:
			[colons add: currentToken start.
			self step.	":"
			verticalBar := true.
			args add: (arg := self parseVariableNode).
			"In case of error, preserve the lone : to ditinguish from error witout even a :"
			arg isParseError ifTrue: [ arg value: ':', arg value ]].
	verticalBar
		ifTrue:
			[(currentToken isBinary and: [currentToken value first = $|])
				ifTrue:
					[node bar: currentToken start.
					self stepBar]
				ifFalse:
					[(currentToken isSpecial: $])
						ifFalse: [ args add:(self parseErrorNode: '''|'' or parameter expected')]]].
	node
		arguments: args;
		colons: colons.
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseCascadeMessage [
	| node receiver messages semicolons err |
	"Parse of the first message. It can be either unary, binary or keyword."
	node := self parseKeywordMessage.

	"Quick check if this matches a cascade. If not, return the already parsed node"
	(currentToken isSpecial: $; )
		ifFalse: [ ^ node ].

	"At this point we know we should create a cascade node."
	messages := OrderedCollection new: 3.
	semicolons := OrderedCollection new: 3.

	"Handle invalid cascade sequences such as:

	(self foo); bar
	7; message

	In this case, the cascade receiver should be an invalid cascade node.
	"
	(node isMessage not or: [ node hasParentheses ]) ifTrue: [
		"Cut parsing with an error.
		If in faulty mode, continue the execution and generate a faulty cascade node.
		Note that this bad cascade node will be added to the list of messages."
		err := self parseErrorNode: 'Message expected'.
		node := ASTInvalidCascadeErrorNode from: err contents: { node }
	].
	receiver := node receiver.
 	messages add: node.

	"Extract the messages following the cascade marker $;"
	[currentToken isSpecial: $;] whileTrue: [
		|newMessage|
		semicolons add: currentToken start.
		newMessage := nil.
		self step.
		self saveCommentsDuring: [
			newMessage := self parseCascadeMessageWithReceiver: receiver ].
		self addCommentsTo: newMessage.
		"Note that newMessage can be a message node or a error node.
		In all cases, it will be added to the list of messages."
		messages add: newMessage ].

	^self cascadeNodeClass messages: messages semicolons: semicolons
]

{ #category : 'private - parsing' }
RBParser >> parseCascadeMessageWithReceiver: receiver [

	| errorNode |
	currentToken isIdentifier
		ifTrue: [ ^ self parseUnaryMessageWith: receiver].

	currentToken isKeyword
		ifTrue: [ ^ self parseKeywordMessageWith: receiver].

	currentToken isLiteralToken ifTrue: [ self patchNegativeLiteral ].
	"Upon encountering an error in the cascade, it stores an error node as it would store a message node and continues the parsing of the cascade.
	This can allow the possibility of fixing the cascade simply by replacing the false node by another."
	currentToken isBinary ifTrue: [
		^ self parseBinaryMessageWith: receiver ].

	"At this point we know this is an invalid cascade.
	Many things can happen next: it could be a closer parenthesis or bracket, the end of the stream, a non-identifier or an empty cascade.

	For now, do not touch the unexpected token and let it be propagated to the caller.
	Maybe someing up the callchain will gladly use it or know how to recover (eg unfinished statement)"
	errorNode := self parseErrorNode: 'Cascade message expected'.
	^ errorNode
]

{ #category : 'parsing' }
RBParser >> parseDoIt [
	"Parse and return an DoIt corresponding to the expression passed as a #source"
	| methodNode |
	methodNode := self doItMethodNodeClass new.
	self parseMethodBodyInto: methodNode.
	^methodNode
]

{ #category : 'private - classes' }
RBParser >> parseEnglobingError: aCollection with: aToken errorMessage: anErrorMessage [
	| firstParse |
	firstParse := self parseErrorNode: anErrorMessage.
	^firstParse isParseError
		ifTrue: [
			(self englobingErrorNodeClass error: aToken withNodes: aCollection)
				value: aToken value asString;
				errorPosition: firstParse errorPosition ]
		ifFalse: [ firstParse ]
]

{ #category : 'private - parsing' }
RBParser >> parseErrorNode: aMessageString [
	| errorNode |
	currentToken isError
		ifTrue: [
			"The current token is an error, consume it and use its content for the error node."
			errorNode := ASTParseErrorNode new
				errorMessage: currentToken cause;
				value: currentToken value;
				start: self errorPosition;
				stop: currentToken stop;
				errorPosition: currentToken location.
			self step.
			^ errorNode].
	"The current token is not an error but is unexpected. Maybe something is missing?
	Anyway, the error node is an empty string and the current token will be used to continue the parsing."
	^ ASTParseErrorNode new
		errorMessage: aMessageString;
		value: '' asString;
		start: self errorPosition;
		stop: self errorPosition - 1;
		errorPosition: self errorPosition
]

{ #category : 'parsing' }
RBParser >> parseExpression [
	"Parse and return an AST node corresponding to the expression passed as a #source"
	| methodNode |
	methodNode := self parseDoIt.
	^(methodNode statements size == 1 and: [ methodNode temporaries isEmpty ])
		  ifTrue: [ methodNode statements first ]
		  ifFalse: [ methodNode body ]
]

{ #category : 'private - parsing' }
RBParser >> parseIncompleteExpression: priorStatementsNode [
	" This method is used when parsing an expression or a method, if the parsing of the list of statements finishes but there are some tokens left to parse."
	 | statements error |
	statements := priorStatementsNode statements copy.
	" Checks if the current token is a closure to create an englobing error node or a parse error node otherwise. "
	error := self getErrorFromClosuresWithMissingOpenings: statements.
	error parent: priorStatementsNode.
	statements add: error.
	self step.
	priorStatementsNode statements: statements.
	^self atEnd ifTrue: [ priorStatementsNode ]
				  ifFalse: [ self parseStatementList: false into: priorStatementsNode untilAnyCloserOf: '' "No extra condition, just until the end of the stream".
								self atEnd ifFalse: [ self parseIncompleteExpression: priorStatementsNode ]
											  ifTrue: [ priorStatementsNode ] ]
]

{ #category : 'private - parsing' }
RBParser >> parseKeywordMessage [
	^self parseKeywordMessageWith: self parseBinaryMessage
]

{ #category : 'private - parsing' }
RBParser >> parseKeywordMessageWith: node [
	| args isKeyword keywordsStartPositions selector|
	args := OrderedCollection new: 3.
	keywordsStartPositions := OrderedCollection new: 3.
	selector := ''.

	isKeyword := false.
	[currentToken isKeyword] whileTrue:
			[keywordsStartPositions add: currentToken start.
			selector := selector, currentToken value.
			self step.
			args add: self parseBinaryMessage.
			isKeyword := true].
	selector := self selectorNodeClass value: selector.
	selector keywordPositions: keywordsStartPositions.
	^isKeyword
		ifTrue:
			[self messageNodeClass
				receiver: node
				selector: selector
				keywordsPositions: keywordsStartPositions
				arguments: args]
		ifFalse: [node]
]

{ #category : 'private - parsing' }
RBParser >> parseKeywordPattern [
	| selector positions args node |
	selector := ''.
	positions := OrderedCollection new: 2.
	args := OrderedCollection new: 2.
	[currentToken isKeyword]
		whileTrue:
			[selector := selector, currentToken value.
			 positions add: currentToken start.
			self step.
			args add: self parseVariableNode].
	node := self methodNodeClass
		selector: selector asSymbol
		keywordsPositions: positions asIntegerArray
		arguments: args.
	node comments: (node comments, args last comments).
	args last comments ifNotEmpty: [ args last comments: #() ].
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseKeywordPragma [
	| selector positions arguments |
	selector := ''.
	positions := OrderedCollection new: 2.
	arguments := OrderedCollection new: 2.
	[ currentToken isKeyword ] whileTrue: [
		selector := selector, currentToken value.
		positions add: currentToken start.
		self step.
		arguments addLast: self parsePragmaLiteral ].
	^ self pragmaNodeClass
		selector: selector asSymbol
		keywordsPositions: positions asIntegerArray
		arguments: arguments
]

{ #category : 'private - parsing' }
RBParser >> parseLiteralArray [
	| literals startToken stop faulty value prevState |
	prevState := scanner state.
	scanner state: #literalArray.
	startToken := currentToken.
	literals := (OrderedCollection new: 5).
	faulty := false.
	self step.
	[self atEnd or: [currentToken isSpecial: $)]]
		whileFalse: [
			literals add: (value := self parseLiteralArrayObject).
			value isParseError ifTrue: [ faulty := true ] ].
	stop := currentToken stop.
	(currentToken isSpecial: $))
		ifFalse: [ ^ self parseEnglobingError: literals with: startToken errorMessage: ''')'' expected'].
	scanner state: prevState.
	self step.
	faulty ifTrue: [
		^ ASTLiteralArrayErrorNode new
			value: startToken value asString;
		 	start: startToken start;
			contents: literals;
			valueAfter: ')';
			stop: stop;
			propertyAt: #notices put: OrderedCollection new;
			yourself ].
	^(self literalArrayNodeClass
		startPosition: startToken start
		contents: literals
		stopPosition: stop
		isByteArray: false) openerSize: startToken length.
]

{ #category : 'private - parsing' }
RBParser >> parseLiteralArrayObject [
	currentToken isSpecial
		ifTrue:
			[currentToken value = $( ifTrue: [^self parseLiteralArray].
			"currentToken value == $[ ifTrue: [^self parseLiteralByteArray]"].
	currentToken isLiteralArrayToken
		ifTrue:
			[^currentToken isForByteArray
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].

	currentToken isError ifTrue:[ |errorNode| errorNode :=  self parseErrorNode: currentToken cause.
		 ^ errorNode].

	"For the moment, special tokens are transformed into symbol."
	(currentToken isAssignment or: [ currentToken isSpecial ])
		ifTrue: [
			|node|
			node := self literalValueNodeClass
				value: currentToken value asSymbol
				start: currentToken start
				stop: currentToken stop.
			node addWarning: 'Use a proper symbol literal'.
			self step. "special"
			^node ].

	^self parsePrimitiveLiteral
]

{ #category : 'private - parsing' }
RBParser >> parseLiteralByteArray [
	"Current token is litteral array token #[ which means following tokens are part of the array
	 until the encounter of the special character ] ."
	| stream startToken value stop faulty |
	startToken := currentToken.
	stream := (Array new: 5) writeStream.
	self step.
	faulty := false.
	[self atEnd or: [currentToken isSpecial: $]]]
		"Put literal value nodes corresponding to the bytes in the stream."
		whileFalse: [
			stream nextPut: (value := self parseLiteralByteArrayObject).
			"In case of unexpected element, continue until the closing bracket, but an error node will sill be produced."
			value isParseError ifTrue: [ faulty := true ]].
	(currentToken isSpecial: $])
		ifFalse: [ ^ self parseEnglobingError: stream contents with: startToken errorMessage: ''']'' expected'].
	stop := currentToken stop.
	self step.
	faulty ifTrue: [
		^ ASTLiteralByteArrayErrorNode new
			value: startToken value asString;
		 	start: startToken start;
			contents: stream contents;
			valueAfter: ']';
			stop: stop;
			propertyAt: #notices put: OrderedCollection new;
			yourself ].
	^(self literalArrayNodeClass
		startPosition: startToken start
		contents: stream contents
		stopPosition: stop
		isByteArray: true) openerSize: startToken length.
]

{ #category : 'private - parsing' }
RBParser >> parseLiteralByteArrayObject [
	"Ensure that the object contained in the array is the value of a byte."
	"Return a literal value node of said byte or a parse error node."
	(currentToken isLiteralToken and:
			[currentToken value isInteger and: [currentToken value between: 0 and: 255]])
		ifFalse: [
			| errorNode lost |
			errorNode := self parseErrorNode: '8-bit integer expected'.
			"Error recovery: consume the token as classic literal array object"
			lost := self parseLiteralArrayObject.
			errorNode value: lost value asString.
			errorNode stop: lost stop.
			^ errorNode ].
	^self parsePrimitiveLiteral
]

{ #category : 'private - parsing' }
RBParser >> parseLiterals [

	| stream |
	stream := (Array new: 5) writeStream.
	[ self atEnd ] whileFalse: [
		| node |
		node := (currentToken isSpecial: $))
			        ifTrue: [
			        self parseErrorNode: 'Unexpected closing parenthesis' ]
			        ifFalse: [ self parseLiteralArrayObject ].
		node isError ifTrue: [ node checkFaulty: nil ].
		stream nextPut: node value ].
	^ stream contents
]

{ #category : 'private - parsing' }
RBParser >> parseMessagePattern [
	currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	^currentToken isIdentifier
		ifTrue: [self parseUnaryPattern]
		ifFalse:
			[currentToken isKeyword
				ifTrue: [self parseKeywordPattern]
				ifFalse: [self parseBinaryPattern]]
]

{ #category : 'parsing' }
RBParser >> parseMethod [
	| methodNode |
	methodNode := self parseMessagePattern.
	self parsePragmas.
	self addCommentsTo: methodNode.
	self parseMethodBodyInto: methodNode.
	^methodNode
]

{ #category : 'private - parsing' }
RBParser >> parseMethodBodyInto: aMethodNode [
	aMethodNode source: source.
	aMethodNode body ifNil: [ aMethodNode body: self sequenceNodeClass new ].

	self parseStatements: true into: aMethodNode body untilAnyCloserOf: #().

	pragmas ifNotNil: [ aMethodNode pragmas: pragmas ].
	self atEnd ifFalse: [ self parseIncompleteExpression: aMethodNode body ].
	(aMethodNode body statements isEmpty and: [currentToken comments isNotNil]) ifTrue: [
		self extractCommentsFrom: currentToken.
		self addCommentsTo: aMethodNode body ]
]

{ #category : 'private - parsing' }
RBParser >> parseParenthesizedExpression [
	| node startToken |
	startToken := currentToken.
	self step.
	node := self parseAssignment.
	^(currentToken isSpecial: $))
		ifTrue:
			[node addParenthesis: (startToken start to: currentToken start).
			self step.
			node]
		ifFalse: [ ^ self parseEnglobingError: (OrderedCollection with: node) with: startToken errorMessage: ''')'' expected']
]

{ #category : 'private - parsing' }
RBParser >> parsePragma [
	"Method called by parsePragmas which already verified that current token is the special character <"

	| startToken pragma |
	startToken := currentToken.
	self step. "<"
	pragma := self basicParsePragma.
	(currentToken isBinary: #>) ifFalse: [
		pragma isPragma ifTrue: [
			pragma
				left: startToken start;
				right: currentToken start-1 ].
		pragma := self
			          parseEnglobingError: (OrderedCollection with: pragma)
			          with: startToken
			          errorMessage: '''>'' expected'.
		^ self addPragma: pragma ].

	pragma
		left: startToken start;
		right: currentToken stop.

	self step. ">"
	self addPragma: pragma
]

{ #category : 'private - parsing' }
RBParser >> parsePragmaAndReturn [

	self parsePragma.
	^ pragmas first
]

{ #category : 'private - parsing' }
RBParser >> parsePragmaLiteral [
	"A literal array token is #( or #[."
	currentToken isLiteralArrayToken
		ifTrue:
			[^currentToken isForByteArray
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isLiteralToken ifFalse: [^self parseErrorNode:'Literal constant expected'].
	"Return a literal value node."
	^self parsePrimitiveLiteral
]

{ #category : 'private - parsing' }
RBParser >> parsePragmas [

	[ currentToken isBinary: #< ]
		whileTrue: [ self parsePragma ]
]

{ #category : 'private - parsing' }
RBParser >> parsePrimitiveIdentifier [
	"Creates a Variable Node and copies the value, start position and comments in said node."
	"This method is called by parseVariableNode which makes sure current token is Identifier."
	| token node |
	token := currentToken.
	self step.
	node := self variableNodeClass identifierNamed: token value at: token start.
	self addCommentsTo: node.
	^node
]

{ #category : 'private - parsing' }
RBParser >> parsePrimitiveKeywordPragma [
	| selector positions arguments |
	selector := ''.
	positions := OrderedCollection new: 2.
	arguments := OrderedCollection new: 2.
	[ currentToken isKeyword ] whileTrue: [
		selector := selector, currentToken value.
		positions add: currentToken start.
		self step.
		arguments addLast: self parsePrimitiveLiteral ].
	^ self pragmaNodeClass
		selector: selector asSymbol
		keywordsPositions: positions asIntegerArray
		arguments: arguments
]

{ #category : 'private - parsing' }
RBParser >> parsePrimitiveLiteral [
	"Creates a literal value node from a literal token."
	| token |

	token := currentToken.
	self step.

	^self literalValueNodeClass
		value: token value
		start: token start
		stop: token stop
		source: token source
]

{ #category : 'private - parsing' }
RBParser >> parsePrimitiveObject [
	| errorNode |
	currentToken isIdentifier ifTrue: [^self parsePrimitiveIdentifier].
	currentToken isLiteralToken ifTrue: [^self parsePrimitiveLiteral].
	currentToken isLiteralArrayToken
		ifTrue:
			[^currentToken isForByteArray
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	(currentToken isSpecial and: ('[({' includes: currentToken value))
		ifTrue:
			[currentToken value = $[ ifTrue: [^self saveCommentsDuring:[self parseBlock]].
			currentToken value = $( ifTrue: [^self parseParenthesizedExpression].
			currentToken value = ${ ifTrue: [^self parseArray]].

	"Annotations are implemented as a magic receiver '@'"
	(currentToken isBinary: #@) ifTrue: [
		| position |
		position := currentToken start.
		self step. "@"
		^ ASTAnnotationMarkNode new start: position ].

	":id is only acceptable at the begin of a block. So just consume them as unexpected"
	((currentToken isSpecial: $:) and: [ self nextToken isIdentifier ])
		ifTrue: [
			| start |
			start := currentToken start.
			self step. ":"
			errorNode := ASTParseErrorNode new
				             errorMessage: 'Unexpected block parameter';
				             start: start;
				             errorPosition: start;
				             stop: currentToken stop;
				             value: ':' , currentToken value.
			self step. "identifier"
			^ errorNode ].

	errorNode := self parseErrorNode: 'Variable or expression expected'.
	^errorNode
]

{ #category : 'private - parsing' }
RBParser >> parseStatementInto: statementList periodList: periods withAcceptedStatementClosers: aCollectionOfClosers [

	"Parse a statement and store it into the statement list.
	Parse statements as valid only if they end in one of the accepted statement closers provided by the enclosing scope.
	E.g., if we are parsing a block, a valid closer is ].

	Put any period found at the end of this statement int the periods collection.
	"
	| returnPosition node startOfStatementToken err |

	"Record the token found at the beginning of the statement.
	If at the end we did not consume it, consume it and mark it as a bad statement for now"
	startOfStatementToken := currentToken.

	"If the current statement starts with ^, parse it as a return + an expression.
	Otherwise just parse it as an expression.
	Expressions start with assignments, which have lower precedence"
	(currentToken isSpecial: $^)
		ifTrue: [
			returnPosition := currentToken start.
			self step.
			node := self returnNodeClass
				        start: returnPosition
				        value: self parseAssignment ]
		ifFalse: [ node := self parseAssignment ].

	"If what follows is a closer that do not match the current scope, consume them and produce the corresponding errors wrapping the statement."
	[(')]}' includes: currentToken value)
		and: [ (aCollectionOfClosers includes: currentToken value) not ]]
			whileTrue: [
				| contents |
				err := self parseErrorNode: 'Missing opener for closer: ', currentToken value asString.
				"If the parsing failed because it did not match anything, there was nothing before the unexpected closer"
				contents := startOfStatementToken = currentToken ifTrue: [ #() ] ifFalse: [ { node } ].
				node := (ASTUnfinishedStatementErrorNode from: err contents: contents)
					valueAfter: currentToken value asString;
					stop: currentToken stop.
				self step ].

	"If the parsing failed because it did not match anything, consume the current token and continue parsing."
	startOfStatementToken = currentToken ifTrue: [
		"We did not progress, its mean that 1. we have a error node and 2. we are missing something (or found something unexpected, its the same thing)."
		"Solution to recover. Consume the token to create the statement."
		node := ASTParseErrorNode new
			errorMessage: 'Unexpected token';
			value: currentToken value asString;
			start: currentToken start;
			stop: currentToken stop;
			errorPosition: currentToken start.
		self step ].

	"Next we should have the end of the statement
	  - a dot or
	  - an accepted scope closer e.g., )]} or
	  - the end of the stream.
	If it is neither, we should mark our statement as unfinished statement.
	And we do not stack unfinished statements."
	(currentToken value ~= $.
		and: [ (aCollectionOfClosers includes: currentToken value) not
			and: [ currentToken isEOF not
				and: [ node isUnfinishedStatement not ] ] ] ) ifTrue: [
		"I do not like the following guard. If currentToken is an unrelated error,
		 then parserError will unfortunately consume and lose it.
		 A better design is needed."
		node isParseError ifFalse: [
			err := self parseErrorNode: 'End of statement expected'.
			node := ASTUnfinishedStatementErrorNode from: err contents: { node } ]
	].

	"Then consume all dots and comments coming after this well formed statement, if there was any"
	(currentToken isSpecial: $.) ifTrue: [
		periods add: currentToken start.
		self step.
		self addCommentsTo: node ].

	[ currentToken isSpecial: $. ]
		whileTrue: [
			periods add: currentToken start.
			self step ].

	"If the previous node in the statement list is a return node, mark this node as unreachable"

	(statementList notEmpty and: [ statementList last isReturn ]) ifTrue: [
		node addWarning: 'Unreachable statement' ].

	"Commit the statement to the statements list"
	statementList add: node
]

{ #category : 'private - parsing' }
RBParser >> parseStatementList: pragmaBoolean into: sequenceNode untilAnyCloserOf: aCollectionOfClosers [

	"Parse a list of statements and set those statetements to a sequence node.
	Stop parsing when we finish the current scope, delimited by a colleciton of closers.
	Different scopes such as blocks or methods can decide different stop conditions."

	| statements return periods |
	return := false.
	statements := sequenceNode statements.
	periods := OrderedCollection new.

	self addCommentsTo: sequenceNode.

	pragmaBoolean ifTrue: [ self parsePragmas ].

	[ currentToken isSpecial: $. ]
		whileTrue: [
			periods add: currentToken start.
			self step ].

	[ self atEnd or: [ currentToken isSpecial
		and: [aCollectionOfClosers includes: currentToken value] ] ]
			whileFalse: [
				self
					parseStatementInto: statements
					periodList: periods
					withAcceptedStatementClosers: aCollectionOfClosers ].

	statements
		ifEmpty: [ self addCommentsTo: sequenceNode ]
		ifNotEmpty: [ self addCommentsTo: statements last ].
	sequenceNode
		statements: statements;
		periods: periods.
	^ sequenceNode
]

{ #category : 'private - parsing' }
RBParser >> parseStatements: pragmaBoolean into: aSequenceNode untilAnyCloserOf: aCollectionOfClosers [

	self parseTemporariesInto: aSequenceNode.

	^self
		parseStatementList: pragmaBoolean
		into: aSequenceNode
		untilAnyCloserOf: aCollectionOfClosers
]

{ #category : 'private - parsing' }
RBParser >> parseTemporariesInto: aSequenceNode [
	| temps leftBar rightBar errorNode startToken |
	temps := OrderedCollection new.
	leftBar := rightBar := nil.
	currentToken isBinary
		ifTrue:
			[currentToken value first = $|
				ifTrue:
					[ startToken := currentToken.
					leftBar := currentToken start.
					self stepBar. "Consume one | only"
					temps := self parseTemps.
					(currentToken isBinary and: [currentToken value first = $|])
						ifFalse: [ errorNode :=  self parseEnglobingError: temps with: startToken errorMessage: '''|'' or variable expected'.
									 aSequenceNode addFaultyNode: errorNode.
									 temps := OrderedCollection new.
									 leftBar := nil.
								    ]
						ifTrue: [
							rightBar := currentToken start.
							self stepBar "Consume one | only" ]]].

	aSequenceNode leftBar: leftBar temporaries: temps rightBar: rightBar
]

{ #category : 'private - parsing' }
RBParser >> parseTemps [
	| temps |
	temps := OrderedCollection new.
	[currentToken isIdentifier] whileTrue: [temps add: self parsePrimitiveIdentifier].
	^temps
]

{ #category : 'private - parsing' }
RBParser >> parseUnaryMessage [
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.
	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isIdentifier]
			whileTrue: [node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node
]

{ #category : 'private - parsing' }
RBParser >> parseUnaryMessageWith: aNode [
	| selector |
	selector := self selectorNodeClass value: currentToken value keywordPositions: {currentToken start}.
	self step.
	^self messageNodeClass
		receiver: aNode
		selector:  selector
		keywordsPositions: selector keywordPositions
		arguments: #()
]

{ #category : 'private - parsing' }
RBParser >> parseUnaryPattern [
	| selector |
	selector := currentToken.
	self step.
	^self methodNodeClass
		selector: selector value asSymbol
		keywordsPositions: {selector start}
		arguments: #()
]

{ #category : 'private - parsing' }
RBParser >> parseUnaryPragma [
	"Current Token must be an Identifier."
	"Return an ASTPragmaNode."
	| selector |
	selector := currentToken.
	self step.
	^ self pragmaNodeClass selector: selector value asSymbol keywordsPositions: (Array with: selector start) arguments: #()
]

{ #category : 'private - parsing' }
RBParser >> parseVariableNode [
	"If called without having an Identifier Token : sends an error.
	 Return a variable node with current token value, start position and comments."

	| errorNode |
	currentToken isIdentifier ifTrue: [ ^ self parsePrimitiveIdentifier ].

	errorNode := self parseErrorNode: 'Variable name expected'.

	"Consume : and optional identifier that follows as an error"
	(currentToken isSpecial: $:) ifTrue: [
		[ currentToken isSpecial: $: ] whileTrue: [
			self step. "consume : while present"
			errorNode stop: currentToken stop.
			errorNode value: errorNode value , ':' ].
		currentToken isIdentifier ifTrue: [ "Also consume the identifier"
			errorNode stop: currentToken stop.
			errorNode value: errorNode value , currentToken source.
			self step ] ].

	^ errorNode
]

{ #category : 'private' }
RBParser >> patchLiteralMessage [

	currentToken value == true
		ifTrue: [ ^ currentToken := ASTIdentifierToken value: 'true' start: currentToken start ].
	currentToken value == false
		ifTrue: [ ^ currentToken := ASTIdentifierToken value: 'false' start: currentToken start ].
	currentToken value
		ifNil: [ ^ currentToken := ASTIdentifierToken value: 'nil' start: currentToken start ]
]

{ #category : 'private' }
RBParser >> patchNegativeLiteral [
	"Handle the special negative number case for binary message sends."

	currentToken value isNumber
		ifFalse: [^self].
	currentToken value <= 0 ifFalse: [^self].
	currentToken value = 0
		ifTrue:
			[(source isNotNil and:
					[source notEmpty
						and: [(source at: (currentToken start min: source size)) = $-]])
				ifFalse: [^self]].
	nextToken := currentToken.
	currentToken := ASTBinarySelectorToken value: #- start: nextToken start.
	nextToken value: nextToken value negated.
	(nextToken isKindOf: ASTNumberLiteralToken)
		ifTrue:
			[nextToken source: (nextToken source allButFirst)].
	nextToken start: nextToken start + 1
]

{ #category : 'private - classes' }
RBParser >> pragmaNodeClass [
	^ ASTPragmaNode
]

{ #category : 'private - classes' }
RBParser >> returnNodeClass [
	^ ASTReturnNode
]

{ #category : 'private - parsing' }
RBParser >> saveCommentsDuring: aBlock [
	| currentComments |
	currentComments := comments copy.
	comments removeAll.
	^ aBlock
		ensure: [ comments := comments, currentComments ]
]

{ #category : 'initialization' }
RBParser >> scanner: aScanner [
	scanner := aScanner.
	pragmas := nil.
	comments := OrderedCollection new.
	self step
]

{ #category : 'accessing' }
RBParser >> scannerClass [
	^RBScanner
]

{ #category : 'private' }
RBParser >> scannerNext [

	^ scanner next
]

{ #category : 'private - classes' }
RBParser >> selectorNodeClass [
	^ ASTSelectorNode
]

{ #category : 'private - classes' }
RBParser >> sequenceNodeClass [
	^ ASTSequenceNode
]

{ #category : 'accessing' }
RBParser >> source: aString [

	self initializeParserWith: aString
]

{ #category : 'private' }
RBParser >> step [
	(currentToken isNotNil and: [currentToken comments isNotNil])
		ifTrue: [self extractCommentsFrom: currentToken].
	nextToken ifNotNil:
			[currentToken := nextToken.
			nextToken := nil]
		ifNil: [currentToken := self scannerNext]
]

{ #category : 'private' }
RBParser >> stepBar [
	"When possible, only consume the first bar (`|`) of the current binary token"

	"All of this because | can be a separator (block args), a type of brackets (temps) and a valid first character of binary operators."

	| value |
	value := currentToken value.
	(value size < 2 or: [ value first ~= $| ]) ifTrue: [ ^ self step ].

	"Because the current token might escape, we need to update the old curent token and create a new current token"
	currentToken value: '|'.
	currentToken := currentToken clone.
	currentToken value: value allButFirst.
	currentToken start: currentToken start + 1
]

{ #category : 'private - classes' }
RBParser >> variableNodeClass [
	^ ASTVariableNode
]
