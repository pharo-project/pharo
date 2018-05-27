"
I'm a simple formatter that prints nodes (without using source code).
 I'm usefull when AST are programmatically assembled. 
I could be improved but the goal is to get the information is a more or less ok form.

"
Class {
	#name : #BISimpleFormatter,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'codeStream',
		'indent',
		'lookaheadCode',
		'originalSource',
		'lineStart'
	],
	#classVars : [
		'FormatAsYouReadPolicy'
	],
	#category : #'AST-Core-Formatter'
}

{ #category : #public }
BISimpleFormatter class >> format: aParseTree [ 
	^self format: aParseTree withIndents: 0
]

{ #category : #public }
BISimpleFormatter class >> format: aParseTree withIndents: anInteger [ 
	^ self new
		indent: anInteger;
		format: aParseTree
]

{ #category : #initialization }
BISimpleFormatter class >> formatAsYouReadPolicy [
	^ FormatAsYouReadPolicy
]

{ #category : #initialization }
BISimpleFormatter class >> formatAsYouReadPolicy: anObject [
	FormatAsYouReadPolicy := anObject
]

{ #category : #initialization }
BISimpleFormatter class >> initialize [
  "self initialize"
  FormatAsYouReadPolicy := false.
 
]

{ #category : #private }
BISimpleFormatter >> addSpaceIfNeededForLastArgument: aPragmaNode [
	aPragmaNode isUnary
		ifTrue: [ ^ self ].
	(self pragmaArgumentNeedsSeparator: aPragmaNode arguments last)
		ifTrue: [ self space ]
]

{ #category : #private }
BISimpleFormatter >> basicFormatCommentFor: aComment [
	codeStream nextPutAll: (originalSource copyFrom: aComment start to: aComment stop).
]

{ #category : #private }
BISimpleFormatter >> bracketWith: bracketString around: aBlock [
	bracketString isEmpty
		ifTrue: [ ^ aBlock value ].
	codeStream nextPut: bracketString first.
	^ aBlock
		ensure: [ codeStream nextPut: bracketString last ] 
]

{ #category : #accessing }
BISimpleFormatter >> codeStream [
	^ codeStream
]

{ #category : #accessing }
BISimpleFormatter >> codeStream: anObject [
	codeStream := anObject
]

{ #category : #'public interface' }
BISimpleFormatter >> containerWidth: anInteger [
	"nothing done on purpose"
]

{ #category : #'public interface' }
BISimpleFormatter >> format: aParseTree [
	originalSource := aParseTree source.
	self visitNode: aParseTree.
	^ codeStream contents
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatArray: anArrayNode [
	anArrayNode statements
		do: [ :each | 
				self visitNode: each ]
		separatedBy: 
				[ codeStream nextPutAll: ' . ' .
				self newLine ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatBlock: aBlockNode [
	self formatBlockArgumentsFor: aBlockNode.
	self space.
	self visitNode: aBlockNode body.
	self space


]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatBlockArgumentsFor: aBlockNode [
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do:
			[ :each | 
			codeStream nextPut: $:.
			self visitNode: each.
			self formatCommentsFor: each.
			self space ].
	codeStream nextPutAll: '| '
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatCommentsFor: aNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aNode comments
		do:
			[ :each | 
			self basicFormatCommentFor: each.
			self newLine ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatMethodBodyFor: aMethodNode [
	self
		indentAround:
			[ 
			self newLine.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatMethodCommentFor: aNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aNode comments
		do:
			[ :each | 
			self basicFormatCommentFor: each.
			self newLine ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatMethodPatternFor: aMethodNode [
	aMethodNode arguments isEmpty
		ifTrue: [ codeStream nextPutAll: aMethodNode selector ]
		ifFalse: [ self privateFormatMethodPatternMonoLineFor: aMethodNode ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatPragmasFor: aMethodNode [
	aMethodNode pragmas do: [ :each | self visitNode: each; newLine ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatSelectorAndArguments: aMessageNode [
	
	self
		indent: 2
		around: [ 
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator: [ self space ]
				restSeparator: [ self space ] ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock [
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

{ #category : #'private-formatting' }
BISimpleFormatter >> formatSequenceNodeStatementsFor: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].
	1 to: statements size do:
		[ :i | 
		self visitNode: (statements at: i).
		i < statements size
			ifTrue: [ codeStream nextPut: $.. 
						self newLine].
		self formatStatementCommentsFor: (statements at: i).
		i < statements size
			ifTrue: [  ] ]
]

{ #category : #'private-formatting' }
BISimpleFormatter >> formatStatementCommentsFor: aStatementNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aStatementNode statementComments
		do:
			[ :each | 
			 self newLine. 
			 self basicFormatCommentFor: each ]
]

{ #category : #private }
BISimpleFormatter >> formatTemporariesFor: aSequenceNode [
	aSequenceNode temporaries isEmpty
		ifTrue: [ ^ self ].
	self
		bracketWith: '|'
		around:
			[ 
			self space.
			aSequenceNode temporaries
				do:
					[ :each | 
					self visitNode: each.
					self formatStatementCommentsFor: each .
					self space ] ].
	self newLine.
]

{ #category : #private }
BISimpleFormatter >> formattedSourceFor: aNode [
	^ lookaheadCode
		at: aNode
		ifAbsentPut: [ 
			self class new
				indent: 1;
				format: aNode ]
]

{ #category : #private }
BISimpleFormatter >> handleLineForArgument: anArgument [
  self space.
  self visitNode: anArgument
]

{ #category : #accessing }
BISimpleFormatter >> indent [
	^ indent
]

{ #category : #accessing }
BISimpleFormatter >> indent: anInteger [

	indent := anInteger
]

{ #category : #private }
BISimpleFormatter >> indent: anInteger around: aBlock [ 
	self indent: self indent + anInteger.
	^aBlock ensure: [self indent: self indent - anInteger]
]

{ #category : #private }
BISimpleFormatter >> indentAround: aBlock [ 
	self indent: 1 around: aBlock
]

{ #category : #accessing }
BISimpleFormatter >> indentString [
	^ '  '
]

{ #category : #initialization }
BISimpleFormatter >> initialize [
	super initialize.
	lineStart := 0.
	self indent: 0.
	lookaheadCode := IdentityDictionary new.
	codeStream := (String new: 256) writeStream.
	
]

{ #category : #accessing }
BISimpleFormatter >> lineStart [
	^ lineStart
]

{ #category : #accessing }
BISimpleFormatter >> lineStart: anObject [
	lineStart := anObject
]

{ #category : #private }
BISimpleFormatter >> needsParenthesisFor: aNode [
  | parent |
  aNode ifNil: [ ^false ].
  aNode isValue ifFalse: [ ^false ].
   parent := aNode parent ifNil: [ ^false ].
   aNode precedence < parent precedence ifTrue: [ ^false ].
   (aNode isAssignment and: [ parent isAssignment ]) ifTrue: [ ^false ].
   (aNode isAssignment and: [ aNode isCascade ]) ifTrue: [ ^true ].
   aNode precedence = 0 ifTrue: [ ^false ].
   aNode isMessage ifFalse: [ ^true ].
   aNode isUnary ifTrue: [ ^false ].
   aNode isKeyword ifTrue: [ ^true ].
   (parent isMessage and: [ parent receiver == aNode ]) ifFalse: [ ^true ].
   aNode precedence = parent precedence ifFalse: [ ^true ].
   ^self precedenceOf: parent selector greaterThan: aNode selector
]

{ #category : #private }
BISimpleFormatter >> newLine [
	self newLines: 1
]

{ #category : #private }
BISimpleFormatter >> newLines: anInteger [
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

{ #category : #private }
BISimpleFormatter >> pragmaArgumentNeedsSeparator: anArgumentNode [
	^ anArgumentNode value isSymbol and: [ anArgumentNode value isBinary ]
]

{ #category : #private }
BISimpleFormatter >> precedenceOf: parentSelector greaterThan: childSelector [
  "Put parenthesis around things that are preceived to have 'lower' precedence. For example, 'a + b * c' 
	-> '(a + b) * c' but 'a * b + c' -> 'a * b + c'"
  | childIndex parentIndex |
  childIndex := 0.
  parentIndex := 0.
  1 to: self traditionalBinaryPrecedenceArray size do: [:i | ((self traditionalBinaryPrecedenceArray at: i) includes: parentSelector first) ifTrue: [ parentIndex := i ].
        ((self traditionalBinaryPrecedenceArray at: i) includes: childSelector first) ifTrue: [ childIndex := i ] ].
  ^childIndex < parentIndex
]

{ #category : #private }
BISimpleFormatter >> privateFormatMethodPatternMonoLineFor: aMethodNode [
	self
		with: aMethodNode selectorParts
		and: aMethodNode arguments
		do: [ :key :arg | 
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self space ]
]

{ #category : #private }
BISimpleFormatter >> space [
	codeStream space
]

{ #category : #accessing }
BISimpleFormatter >> traditionalBinaryPrecedenceArray [
	^  #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@))
]

{ #category : #visiting }
BISimpleFormatter >> visitArrayNode: anArrayNode [
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]
]

{ #category : #visiting }
BISimpleFormatter >> visitAssignmentNode: anAssignmentNode [ 
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self visitNode: anAssignmentNode value
]

{ #category : #visiting }
BISimpleFormatter >> visitBlockNode: aBlockNode [
	self
		bracketWith: '[]'
		around: [ self formatBlock: aBlockNode ].
	
]

{ #category : #visiting }
BISimpleFormatter >> visitCascadeNode: aCascadeNode [

	self visitNode: aCascadeNode receiver.
	self
		indentAround:
			[ 
			self newLine.
			aCascadeNode messages
				do:
					[ :each | self indentAround: [ self formatSelectorAndArguments: each firstSeparator: [  ] restSeparator: [ self space ] ] ]
				separatedBy: [ 
					codeStream nextPut: $;.
					self newLine ] ].
	
]

{ #category : #visiting }
BISimpleFormatter >> visitLiteralArrayNode: aRBArrayLiteralNode [ 
	| brackets |
	codeStream nextPut: $#.
	brackets := aRBArrayLiteralNode isForByteArray 
				ifTrue: ['[]']
				ifFalse: ['()'].
	self bracketWith: brackets
		around: 
			[aRBArrayLiteralNode contents do: [:each | self visitNode: each]
				separatedBy: [self space]]
]

{ #category : #visiting }
BISimpleFormatter >> visitLiteralNode: aLiteralNode [
	aLiteralNode value isLiteral
		ifFalse: [ 

			self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString: aLiteralNode sourceText ]
]

{ #category : #visiting }
BISimpleFormatter >> visitMessageNode: aMessageNode [ 
	self visitNode: aMessageNode receiver.
	self formatSelectorAndArguments: aMessageNode
]

{ #category : #visiting }
BISimpleFormatter >> visitMethodNode: aMethodNode [ 
	self formatMethodPatternFor: aMethodNode.
	self formatMethodBodyFor: aMethodNode
]

{ #category : #visiting }
BISimpleFormatter >> visitNode: aNode [
	| needsParenthesis |
	(lookaheadCode includesKey: aNode)
		ifTrue: [ ^ self writeString: (lookaheadCode at: aNode) ].
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: [ '()' ]
				ifFalse: [ '' ])
		around: [ super visitNode: aNode ]
]

{ #category : #visiting }
BISimpleFormatter >> visitPatternBlockNode: aRBPatternBlockNode [ 
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternBlockNode]
]

{ #category : #visiting }
BISimpleFormatter >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternWrapperBlockNode]
]

{ #category : #visiting }
BISimpleFormatter >> visitPragmaNode: aPragmaNode [
	codeStream nextPut: $<.
	self 
		formatSelectorAndArguments: aPragmaNode 
		firstSeparator: [ 
			aPragmaNode selector isInfix
				ifTrue: [ self space ] ]
		restSeparator: [ self space ].
	self addSpaceIfNeededForLastArgument: aPragmaNode.
	codeStream nextPut: $>
]

{ #category : #visiting }
BISimpleFormatter >> visitReturnNode: aReturnNode [ 
	codeStream nextPut: $^.
	self visitNode: aReturnNode value
]

{ #category : #visiting }
BISimpleFormatter >> visitSequenceNode: aSequenceNode [
	self formatTemporariesFor: aSequenceNode.
	self formatCommentsFor: aSequenceNode.
	self formatSequenceNodeStatementsFor: aSequenceNode
]

{ #category : #visiting }
BISimpleFormatter >> visitVariableNode: aVariableNode [ 
	codeStream nextPutAll: aVariableNode name
]

{ #category : #utilities }
BISimpleFormatter >> with: firstCollection and: secondCollection do: aBlock separatedBy: separatorBlock [
	firstCollection isEmpty
		ifTrue: [ ^ self ].
	aBlock value: firstCollection first value: secondCollection first.
	2 to: firstCollection size do: [ :i | 
		separatorBlock value.
		aBlock value: (firstCollection at: i) value: (secondCollection at: i) ]
]

{ #category : #private }
BISimpleFormatter >> writeString: aString [
	| index |
	index := aString lastIndexOf: Character cr ifAbsent: [ 0 ].
	 codeStream nextPutAll: aString .
	index > 0
		ifTrue: [ lineStart := codeStream position - (aString size - index) ]
]
