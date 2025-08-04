"
I'm a simple formatter that prints nodes (without using source code).
I'm useful when AST are programmatically assembled. 

I could be improved, but the goal is to get the information is a more or less ok form.

"
Class {
	#name : 'OCSimpleFormatter',
	#superclass : 'OCAbstractFormatter',
	#instVars : [
		'codeStream',
		'indent',
		'lineStart'
	],
	#classVars : [
		'FormatAsYouReadPolicy'
	],
	#category : 'AST-Core-Formatter',
	#package : 'AST-Core',
	#tag : 'Formatter'
}

{ #category : 'public' }
OCSimpleFormatter class >> format: aParseTree [
	^self format: aParseTree withIndents: 0
]

{ #category : 'public' }
OCSimpleFormatter class >> format: aParseTree withIndents: anInteger [
	^ self new
		indent: anInteger;
		format: aParseTree
]

{ #category : 'accessing' }
OCSimpleFormatter class >> formatAsYouReadPolicy [
	^ FormatAsYouReadPolicy
]

{ #category : 'accessing' }
OCSimpleFormatter class >> formatAsYouReadPolicy: anObject [
	FormatAsYouReadPolicy := anObject
]

{ #category : 'class initialization' }
OCSimpleFormatter class >> initialize [

  FormatAsYouReadPolicy := false
]

{ #category : 'priority' }
OCSimpleFormatter class >> priority [
	^ 0
]

{ #category : 'private' }
OCSimpleFormatter >> addSpaceIfNeededForLastArgument: aPragmaNode [
	aPragmaNode isUnary
		ifTrue: [ ^ self ].
	(self pragmaArgumentNeedsSeparator: aPragmaNode arguments last)
		ifTrue: [ self space ]
]

{ #category : 'private' }
OCSimpleFormatter >> basicFormatCommentFor: aComment [
	codeStream
		nextPut: $";
		nextPutAll: aComment contents;
		nextPut: $"
]

{ #category : 'private' }
OCSimpleFormatter >> bracketWith: bracketString around: aBlock [
	bracketString isEmpty
		ifTrue: [ ^ aBlock value ].
	codeStream nextPut: bracketString first.
	^ aBlock
		ensure: [ codeStream nextPut: bracketString last ]
]

{ #category : 'accessing' }
OCSimpleFormatter >> codeStream [
	^ codeStream
]

{ #category : 'accessing' }
OCSimpleFormatter >> codeStream: anObject [
	codeStream := anObject
]

{ #category : 'public interface' }
OCSimpleFormatter >> format: aParseTree [
	self visitNode: aParseTree.
	^ codeStream contents
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatArray: anArrayNode [
	anArrayNode statements
		do: [ :each |
				self visitNode: each ]
		separatedBy:
				[ codeStream nextPutAll: ' . '.
				self newLine ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatBlock: aBlockNode [
	self
		formatBlockArgumentsFor: aBlockNode;
		space;
		visitNode: aBlockNode body;
		space
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatBlockArgumentsFor: aBlockNode [
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do: [ :each |
				each isParseError ifFalse: [ codeStream nextPut: $: ].
			   self
					visitNode: each;
			   		formatCommentsFor: each;
					space ].
	codeStream nextPutAll: '| '
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatCommentsFor: aNode [

	aNode comments do: [ :each |
			self
				basicFormatCommentFor: each;
				newLine
			]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatMethodBodyFor: aMethodNode [
	self
		indentAround: [
			self
				newLine;
				formatMethodCommentFor: aMethodNode;
				formatPragmasFor: aMethodNode;
				visitNode: aMethodNode body ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatMethodCommentFor: aNode [

	aNode comments do: [ :each |
			self
				basicFormatCommentFor: each;
				newLine
			]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatMethodPatternFor: aMethodNode [
	aMethodNode arguments isEmpty
		ifTrue: [ codeStream nextPutAll: aMethodNode selector ]
		ifFalse: [ self privateFormatMethodPatternMonoLineFor: aMethodNode ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatPragmasFor: aMethodNode [
	aMethodNode pragmas do: [:each | self visitNode: each; newLine ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatSelectorAndArguments: aMessageNode [

	self
		indent: 2
		around: [
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator: [ self space ]
				restSeparator: [ self space ] ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock [
	aMessageNode isUnary
		ifTrue: [
			self space.
			codeStream nextPutAll: aMessageNode selector ]
		ifFalse: [
			aMessageNode selectorParts
				with: aMessageNode arguments
				do: [ :selector :argument |
					self space.
					self
						indentAround: [
							codeStream nextPutAll: selector.
							self handleLineForArgument: argument ] ] ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatSequenceNodeStatementsFor: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].

	statements doWithIndex: [ :el :i |
		self visitNode: (statements at: i).
		i < statements size
			ifTrue: [ codeStream nextPut: $..
						self newLine].
		self formatStatementCommentsFor: el.
		i < statements size
			ifTrue: [  ] ]
]

{ #category : 'private - formatting' }
OCSimpleFormatter >> formatStatementCommentsFor: aStatementNode [

	aStatementNode statementComments do: [ :each |
			self
				newLine;
				basicFormatCommentFor: each
			]
]

{ #category : 'private' }
OCSimpleFormatter >> formatTemporariesFor: aSequenceNode [
	aSequenceNode hasTemporaries
		ifFalse: [ ^ self ].
	self
		bracketWith: '|'
		around: [
			self space.
			aSequenceNode temporaries
				do:
					[ :each |
					self
						visitNode: each;
						formatStatementCommentsFor: each;
						space ]].
	self newLine
]

{ #category : 'private' }
OCSimpleFormatter >> handleLineForArgument: anArgument [

  self
		space;
		visitNode: anArgument
]

{ #category : 'accessing' }
OCSimpleFormatter >> indent [
	^ indent
]

{ #category : 'accessing' }
OCSimpleFormatter >> indent: anInteger [

	indent := anInteger
]

{ #category : 'private' }
OCSimpleFormatter >> indent: anInteger around: aBlock [
	self indent: self indent + anInteger.
	^ aBlock
		ensure: [ self indent: self indent - anInteger ]
]

{ #category : 'private' }
OCSimpleFormatter >> indentAround: aBlock [
	self indent: 1 around: aBlock
]

{ #category : 'accessing' }
OCSimpleFormatter >> indentString [
	^ '  '
]

{ #category : 'initialization' }
OCSimpleFormatter >> initialize [
	super initialize.
	lineStart := 0.
	self indent: 0.
	codeStream := (String new: 256) writeStream
]

{ #category : 'accessing' }
OCSimpleFormatter >> lineStart [
	^ lineStart
]

{ #category : 'accessing' }
OCSimpleFormatter >> lineStart: anObject [
	lineStart := anObject
]

{ #category : 'private' }
OCSimpleFormatter >> needsParenthesisFor: aNode [
	| parent |
	aNode ifNil: [ ^ false ].
	aNode isValue
		ifFalse: [ ^ false ].
	aNode isParseError
		ifTrue: [ ^ aNode hasParentheses ].
	parent := aNode parent ifNil: [ ^ false ].
	aNode precedence < parent precedence
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ parent isAssignment ])
		ifTrue: [ ^ false ].
	(aNode isAssignment and: [ aNode isCascade ])
		ifTrue: [ ^ true ].
	aNode precedence = 0
		ifTrue: [ ^ false ].
	aNode isMessage
		ifFalse: [ ^ true ].
	aNode isUnary
		ifTrue: [ ^ false ].
	aNode isKeyword
		ifTrue: [ ^ true ].
	(parent isMessage and: [ parent receiver == aNode ])
		ifFalse: [ ^ true ].
	aNode precedence = parent precedence
		ifFalse: [ ^ true ].
	^ self precedenceOf: parent selector greaterThan: aNode selector
]

{ #category : 'private' }
OCSimpleFormatter >> newLine [
	self newLines: 1
]

{ #category : 'private' }
OCSimpleFormatter >> newLines: anInteger [
	anInteger + self indentString size = 0
		ifTrue:
			[ codeStream space ].
	anInteger
		timesRepeat:
			[ codeStream cr ].
	lineStart := codeStream position.
	self
		indent
		timesRepeat:
			[ codeStream nextPutAll: self indentString ]
]

{ #category : 'private' }
OCSimpleFormatter >> pragmaArgumentNeedsSeparator: anArgumentNode [
	^ anArgumentNode value isSymbol and: [ anArgumentNode value isBinary ]
]

{ #category : 'private' }
OCSimpleFormatter >> precedenceOf: parentSelector greaterThan: childSelector [
  "Put parenthesis around things that are preceived to have 'lower' precedence. For example, 'a + b * c'
	-> '(a + b) * c' but 'a * b + c' -> 'a * b + c'"
  | childIndex parentIndex |
  childIndex := 0.
  parentIndex := 0.
  1 to: self traditionalBinaryPrecedenceArray size do: [:i | ((self traditionalBinaryPrecedenceArray at: i) includes: parentSelector first) ifTrue: [ parentIndex := i ].
        ((self traditionalBinaryPrecedenceArray at: i) includes: childSelector first) ifTrue: [ childIndex := i ] ].
  ^childIndex < parentIndex
]

{ #category : 'private' }
OCSimpleFormatter >> privateFormatMethodPatternMonoLineFor: aMethodNode [
	self
		with: aMethodNode selectorParts
		and: aMethodNode arguments
		do: [:key :arg |
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self space ]
]

{ #category : 'private' }
OCSimpleFormatter >> space [
	codeStream space
]

{ #category : 'accessing' }
OCSimpleFormatter >> traditionalBinaryPrecedenceArray [
	^  #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@))
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitAnnotationMarkNode: aRBAnnotationValueNode [

	codeStream nextPut: $@
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitArrayNode: anArrayNode [
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self visitNode: anAssignmentNode value
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitBlockNode: aBlockNode [
	self
		bracketWith: '[]'
		around: [ self formatBlock: aBlockNode ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitCascadeNode: aCascadeNode [
	self visitNode: aCascadeNode receiver.
	self
		indentAround: [ self newLine.
			aCascadeNode messages
				do: [ :each |
					self
						indentAround: [ self
								formatSelectorAndArguments: each
								firstSeparator: [  ]
								restSeparator: [ self space ] ] ]
				separatedBy: [ codeStream nextPut: $;.
					self newLine ] ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitEnglobingErrorNode: aNode [

	aNode isTemporariesError ifTrue: [
		^ self formatTemporariesFor: aNode ].
	self writeString: aNode value.
	self formatBlockArgumentsFor: aNode.
	aNode contents do: [ :each | self visitNode: each ] separatedBy: [
		codeStream nextPutAll: ' '.
		self newLine ].
	self writeString: aNode valueAfter
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitLiteralArrayNode: aRBArrayLiteralNode [
	| brackets |
	codeStream nextPut: $#.
	brackets := aRBArrayLiteralNode isForByteArray
		ifTrue: [ '[]' ]
		ifFalse: [ '()' ].
	self
		bracketWith: brackets
		around: [ aRBArrayLiteralNode contents
				do: [ :each | self visitNode: each ]
				separatedBy: [ self space ] ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitLiteralNode: aLiteralNode [
	self writeString: aLiteralNode sourceText
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitMessageNode: aMessageNode [
	self
		visitNode: aMessageNode receiver;
		formatSelectorAndArguments: aMessageNode
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitMethodNode: aMethodNode [
	self
		formatMethodPatternFor: aMethodNode;
		formatMethodBodyFor: aMethodNode
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitNode: aNode [
	| needsParenthesis |
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: [ '()' ]
				ifFalse: [ '' ])
		around: [ super visitNode: aNode ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitParseErrorNode: aNode [
		self writeString: aNode value
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitPatternBlockNode: aPatternBlockNode [
	codeStream nextPut: $`.
	self
		bracketWith: '{}'
		around: [ self formatBlock: aPatternBlockNode ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitPatternWrapperBlockNode: aPatternWrapperBlockNode [
	self visitNode: aPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self
		bracketWith: '{}'
		around: [ self formatBlock: aPatternWrapperBlockNode ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitPragmaNode: aPragmaNode [

	| hackNoBrackets |
	"If the parent is an error, the '<' and '>' are already taken care of."
	hackNoBrackets := aPragmaNode parent isNotNil and: [ aPragmaNode parent isEnglobingError ].

	hackNoBrackets ifFalse: [ codeStream nextPut: $< ].
	self
		formatSelectorAndArguments: aPragmaNode
		firstSeparator: [
			aPragmaNode selector isInfix
				ifTrue: [ self space ] ]
		restSeparator: [ self space ].
	self addSpaceIfNeededForLastArgument: aPragmaNode.
	hackNoBrackets ifFalse: [ codeStream nextPut: $> ]
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitReturnNode: aReturnNode [
	codeStream nextPut: $^.
	self visitNode: aReturnNode value
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitSequenceNode: aSequenceNode [

	self
		formatTemporariesFor: aSequenceNode;
		formatCommentsFor: aSequenceNode;
		formatSequenceNodeStatementsFor: aSequenceNode
]

{ #category : 'visiting' }
OCSimpleFormatter >> visitVariableNode: aVariableNode [
	codeStream nextPutAll: aVariableNode name
]

{ #category : 'utilities' }
OCSimpleFormatter >> with: firstCollection and: secondCollection do: aBlock separatedBy: separatorBlock [
	firstCollection isEmpty
		ifTrue: [ ^ self ].
	aBlock value: firstCollection first value: secondCollection first.
	2 to: firstCollection size do: [ :i |
		separatorBlock value.
		aBlock value: (firstCollection at: i) value: (secondCollection at: i) ]
]

{ #category : 'private' }
OCSimpleFormatter >> writeString: aString [
	| index |
	index := aString lastIndexOf: Character cr ifAbsent: [ 0 ].
	 codeStream nextPutAll: aString .
	index > 0
		ifTrue: [ lineStart := codeStream position - (aString size - index) ]
]
