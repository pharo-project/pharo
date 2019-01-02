"
BIConfigurableFormatter formats the Refactoring Browser's parse trees. It has many more formatting options than the default formatter used by the RB. To change the RB to use this formatter, execute ""RBProgramNode formatterClass: BIConfigurableFormatter"". For some refactorings the RB must reformat the code after the change, so it is good to have a formatter configured to your tastes.

Instance Variables:
	codeStream	<PositionableStream>	the stream we are writing our output to
	indent	<Integer>	how many times are we indenting a new line -- indents are normally tabs but could be any whitespace string
	lineStart	<Integer>	the position of the character that started the current line. This is used for calculating the line length.
	lookaheadCode	<Dictionary key: RBProgramNode value: String>	sometimes we need to lookahead while formatting, this dictionary contains the nodes that have already been formatted by lookahead
	originalSource	<String>	the original source before we started formatting. This is used to extract the comments from the original source.

BIConfigurableFormatter new
"
Class {
	#name : #BIConfigurableFormatter,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'codeStream',
		'indent',
		'lookaheadCode',
		'originalSource',
		'lineStart',
		'context',
		'isInCascadeNode'
	],
	#classVars : [
		'DefaultPrettyPrintContext',
		'FormatAsYouReadPolicy'
	],
	#category : #'BlueInk-Core-Base'
}

{ #category : #private }
BIConfigurableFormatter class >> announceASettingChange [
	SystemAnnouncer uniqueInstance announce: BISettingsChanged 
]

{ #category : #public }
BIConfigurableFormatter class >> format: aParseTree [ 
	^self format: aParseTree withIndents: 0
]

{ #category : #public }
BIConfigurableFormatter class >> format: aParseTree withIndents: anInteger [ 
	^ self new
		indent: anInteger;
		format: aParseTree
]

{ #category : #accessing }
BIConfigurableFormatter class >> formatAsYouReadPolicy [
	^ FormatAsYouReadPolicy
]

{ #category : #accessing }
BIConfigurableFormatter class >> formatAsYouReadPolicy: anObject [
	FormatAsYouReadPolicy := anObject
]

{ #category : #accessing }
BIConfigurableFormatter class >> formatCommentWithStatements [
	^ DefaultPrettyPrintContext formatCommentWithStatements
]

{ #category : #accessing }
BIConfigurableFormatter class >> formatCommentWithStatements: aBoolean [
	DefaultPrettyPrintContext formatCommentWithStatements: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> indentString [
	^ DefaultPrettyPrintContext indentString
]

{ #category : #accessing }
BIConfigurableFormatter class >> indentString: aString [
	DefaultPrettyPrintContext indentString: aString.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> indentsForKeywords [
	^ DefaultPrettyPrintContext indentsForKeywords
]

{ #category : #accessing }
BIConfigurableFormatter class >> indentsForKeywords: anInteger [
	DefaultPrettyPrintContext indentsForKeywords: anInteger.
	self announceASettingChange
]

{ #category : #initialization }
BIConfigurableFormatter class >> initialize [
	"self initialize"

	FormatAsYouReadPolicy := false.
	DefaultPrettyPrintContext := BIPrettyPrinterContext new
]

{ #category : #accessing }
BIConfigurableFormatter class >> keepBlockInMessage [
	^ DefaultPrettyPrintContext keepBlockInMessage
]

{ #category : #accessing }
BIConfigurableFormatter class >> keepBlockInMessage: aBoolean [
	DefaultPrettyPrintContext keepBlockInMessage: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> lineUpBlockBrackets [
	^ DefaultPrettyPrintContext lineUpBlockBrackets
]

{ #category : #accessing }
BIConfigurableFormatter class >> lineUpBlockBrackets: aBoolean [
	DefaultPrettyPrintContext lineUpBlockBrackets: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> maxLineLength [
	^ DefaultPrettyPrintContext maxLineLength
]

{ #category : #accessing }
BIConfigurableFormatter class >> maxLineLength: anInteger [
	DefaultPrettyPrintContext maxLineLength: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> methodSignatureOnMultipleLines [
	^ DefaultPrettyPrintContext methodSignatureOnMultipleLines
]

{ #category : #accessing }
BIConfigurableFormatter class >> methodSignatureOnMultipleLines: aBoolean [
	DefaultPrettyPrintContext methodSignatureOnMultipleLines: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> minimumNewLinesBetweenStatements [
	^ DefaultPrettyPrintContext minimumNewLinesBetweenStatements
]

{ #category : #accessing }
BIConfigurableFormatter class >> minimumNewLinesBetweenStatements: anInteger [
	DefaultPrettyPrintContext minimumNewLinesBetweenStatements: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> multiLineMessages [
	^ DefaultPrettyPrintContext multiLineMessages printString
]

{ #category : #accessing }
BIConfigurableFormatter class >> multiLineMessages: aString [
	DefaultPrettyPrintContext multiLineMessages: (self compiler evaluate: aString).
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineAfterCascade [
	^ DefaultPrettyPrintContext newLineAfterCascade
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineAfterCascade: aBoolean [
	DefaultPrettyPrintContext newLineAfterCascade: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineBeforeFirstCascade [
	^ DefaultPrettyPrintContext newLineBeforeFirstCascade
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineBeforeFirstCascade: aBoolean [
	DefaultPrettyPrintContext newLineBeforeFirstCascade: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineBeforeFirstKeyword [
	^ DefaultPrettyPrintContext newLineBeforeFirstKeyword
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLineBeforeFirstKeyword: aBoolean [
	DefaultPrettyPrintContext newLineBeforeFirstKeyword: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterMethodComment [
	^ DefaultPrettyPrintContext newLinesAfterMethodComment
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterMethodComment: anInteger [
	DefaultPrettyPrintContext newLinesAfterMethodComment: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterMethodPattern [
	^ DefaultPrettyPrintContext newLinesAfterMethodPattern
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterMethodPattern: anInteger [
	DefaultPrettyPrintContext newLinesAfterMethodPattern: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterTemporaries [
	^ DefaultPrettyPrintContext newLinesAfterTemporaries
]

{ #category : #accessing }
BIConfigurableFormatter class >> newLinesAfterTemporaries: anInteger [
	DefaultPrettyPrintContext newLinesAfterTemporaries: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> numberOfArgumentsForMultiLine [
	^ DefaultPrettyPrintContext numberOfArgumentsForMultiLine
]

{ #category : #accessing }
BIConfigurableFormatter class >> numberOfArgumentsForMultiLine: anInteger [
	DefaultPrettyPrintContext numberOfArgumentsForMultiLine: anInteger.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> oneLineMessages [
	^ DefaultPrettyPrintContext oneLineMessages printString
]

{ #category : #accessing }
BIConfigurableFormatter class >> oneLineMessages: aString [
	DefaultPrettyPrintContext oneLineMessages: (self compiler evaluate: aString).
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAsTerminators [
	^ DefaultPrettyPrintContext periodsAtEndOfBlock and: [ DefaultPrettyPrintContext periodsAtEndOfMethod ]
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAsTerminators: aBoolean [
	DefaultPrettyPrintContext periodsAtEndOfBlock: aBoolean.
	DefaultPrettyPrintContext periodsAtEndOfMethod: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAtEndOfBlock [
	^ DefaultPrettyPrintContext periodsAtEndOfBlock
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAtEndOfBlock: aBoolean [
	DefaultPrettyPrintContext periodsAtEndOfBlock: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAtEndOfMethod [
	^ DefaultPrettyPrintContext periodsAtEndOfMethod
]

{ #category : #accessing }
BIConfigurableFormatter class >> periodsAtEndOfMethod: aBoolean [
	DefaultPrettyPrintContext periodsAtEndOfMethod: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> retainBlankLinesBeforeComments [
	^ DefaultPrettyPrintContext retainBlankLinesBeforeComments
]

{ #category : #accessing }
BIConfigurableFormatter class >> retainBlankLinesBeforeComments: aBoolean [
	DefaultPrettyPrintContext retainBlankLinesBeforeComments: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> retainBlankLinesBetweenStatements [
	^ DefaultPrettyPrintContext retainBlankLinesBetweenStatements
]

{ #category : #accessing }
BIConfigurableFormatter class >> retainBlankLinesBetweenStatements: aBoolean [
	DefaultPrettyPrintContext retainBlankLinesBetweenStatements: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> selectorAndArgumentCombinedMaxSize [
	^ DefaultPrettyPrintContext selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
BIConfigurableFormatter class >> selectorAndArgumentCombinedMaxSize: anInteger [
	DefaultPrettyPrintContext selectorAndArgumentCombinedMaxSize: anInteger.
	self announceASettingChange
]

{ #category : #settings }
BIConfigurableFormatter class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #blueInkFormatter)
		target: self;
		parent: #codeBrowsing;
		label: 'BlueInk Pretty Printing';
		description: 'Settings related to the formatter';
		with:
				[ 
			(aBuilder setting: #formatCommentWithStatements) label: 'Format comment with statements'.
			(aBuilder setting: #indentString) label: 'Indent string'.
			(aBuilder setting: #indentsForKeywords) label: 'Indents for keywords'.
			(aBuilder setting: #keepBlockInMessage)
				label: 'Keep block in message';
				description: (BIFormatSettingsExample >> #keepBlockInMessageExample) sourceCode.
			(aBuilder setting: #lineUpBlockBrackets)
				label: 'Line up block brackets';
				description: (BIFormatSettingsExample >> #lineUpBlockBracketsExample) sourceCode.
			(aBuilder setting: #methodSignatureOnMultipleLines)
				label: 'Method signature on multiple lines'.
			(aBuilder setting: #oneLineMessages) label: 'One line messages'.
			(aBuilder setting: #multiLineMessages) label: 'Multi line messages'.
			(aBuilder setting: #minimumNewLinesBetweenStatements)
				label: 'Minimum new lines between statements'.
			(aBuilder setting: #newLineAfterCascade)
				label: 'New line after cascade';
				description: (BIFormatSettingsExample >> #newLineAfterCascadeExample) sourceCode.
			(aBuilder setting: #newLineBeforeFirstCascade)
				label: 'New line before first cascade';
				description: (BIFormatSettingsExample >> #newLineBeforeFirstCascadeExample) sourceCode.
			(aBuilder setting: #newLineBeforeFirstKeyword)
				label: 'New line before first keyword';
				description: (BIFormatSettingsExample >> #newLineBeforeFirstKeywordExample) sourceCode.
			(aBuilder setting: #newLinesAfterMethodComment) label: 'New lines after method comment'.
			(aBuilder setting: #newLinesAfterMethodPattern) label: 'New lines after method pattern'.
			(aBuilder setting: #newLinesAfterTemporaries) label: 'New lines after temporaries'.
			(aBuilder setting: #numberOfArgumentsForMultiLine)
				label: 'Number of arguments for multi line'.
			(aBuilder setting: #periodsAsTerminators) label: 'Periods as terminators'.
			(aBuilder setting: #periodsAtEndOfBlock) label: 'Periods at end of block'.
			(aBuilder setting: #periodsAtEndOfMethod) label: 'Periods at end of method'.
			(aBuilder setting: #retainBlankLinesBeforeComments)
				label: 'Retain blank lines before comments'.
			(aBuilder setting: #retainBlankLinesBetweenStatements)
				label: 'Retain blank lines between statements'.
			(aBuilder setting: #selectorAndArgumentCombinedMaxSize)
				label: 'Selector and argument combined in method signature max size'.
			(aBuilder setting: #stringFollowingReturn) label: 'String following return'.
			(aBuilder setting: #stringInsideBlocks) label: 'String inside blocks'.
			(aBuilder setting: #stringInsideParentheses) label: 'String inside parentheses'.
			(aBuilder setting: #traditionalBinaryPrecedenceArray) label: 'Traditional binary precedence'.
			(aBuilder setting: #useBasicCommentFormat) label: 'Use basic comment format'.
			(aBuilder setting: #useTraditionalBinaryPrecedenceForParentheses)
				label: 'Use traditional binary precedence for parentheses' ]
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringFollowingReturn [
	^ DefaultPrettyPrintContext stringFollowingReturn
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringFollowingReturn: aString [
	DefaultPrettyPrintContext stringFollowingReturn: aString.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringInsideBlocks [
	^ DefaultPrettyPrintContext stringInsideBlocks
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringInsideBlocks: aString [
	DefaultPrettyPrintContext stringInsideBlocks: aString.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringInsideParentheses [
	^ DefaultPrettyPrintContext stringInsideParentheses
]

{ #category : #accessing }
BIConfigurableFormatter class >> stringInsideParentheses: aString [
	DefaultPrettyPrintContext stringInsideParentheses: aString.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> traditionalBinaryPrecedenceArray [
	^ DefaultPrettyPrintContext traditionalBinaryPrecedenceArray printString 
]

{ #category : #accessing }
BIConfigurableFormatter class >> traditionalBinaryPrecedenceArray: aString [
	DefaultPrettyPrintContext traditionalBinaryPrecedenceArray: (self compiler evaluate: aString).
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> useBasicCommentFormat [
	^ DefaultPrettyPrintContext useBasicCommentFormat
]

{ #category : #accessing }
BIConfigurableFormatter class >> useBasicCommentFormat: aBoolean [
	DefaultPrettyPrintContext useBasicCommentFormat: aBoolean.
	self announceASettingChange
]

{ #category : #accessing }
BIConfigurableFormatter class >> useTraditionalBinaryPrecedenceForParentheses [
	^ DefaultPrettyPrintContext useTraditionalBinaryPrecedenceForParentheses
]

{ #category : #accessing }
BIConfigurableFormatter class >> useTraditionalBinaryPrecedenceForParentheses: aBoolean [
	DefaultPrettyPrintContext useTraditionalBinaryPrecedenceForParentheses: aBoolean.
	self announceASettingChange
]

{ #category : #private }
BIConfigurableFormatter >> addNewLinesBeforeStatementStartingAt: anInteger [ 
	| newLines |
	newLines := self minimumNewLinesBetweenStatements 
				max: (self retainBlankLinesBetweenStatements 
						ifTrue: [self newLinesBeforeStartingAt: anInteger]
						ifFalse: [0]).
	newLines = 0 ifTrue: [self space] ifFalse: [self newLines: newLines] 
]

{ #category : #private }
BIConfigurableFormatter >> addSpaceIfNeededForLastArgument: aPragmaNode [
	aPragmaNode isUnary
		ifTrue: [ ^ self ].
	(self pragmaArgumentNeedsSeparator: aPragmaNode arguments last)
		ifTrue: [ self space ]
]

{ #category : #private }
BIConfigurableFormatter >> basicFormatCommentFor: aComment [
	codeStream nextPutAll: (originalSource copyFrom: aComment start to: aComment stop).
]

{ #category : #private }
BIConfigurableFormatter >> bracketWith: bracketString around: aBlock [
	bracketString isEmpty
		ifTrue: [ ^ aBlock value ].
	codeStream nextPut: bracketString first.
	^ aBlock
		ensure: [ codeStream nextPut: bracketString last ] 
]

{ #category : #private }
BIConfigurableFormatter >> characterSeparatorMethodSignatureFor: aMethodNode [
	^ (self needsMethodSignatureOnMultipleLinesFor: aMethodNode)
			ifTrue: [ self newLine ]
			ifFalse: [ self space ]
]

{ #category : #accessing }
BIConfigurableFormatter >> codeStream [
	^ codeStream
]

{ #category : #accessing }
BIConfigurableFormatter >> codeStream: anObject [
	codeStream := anObject
]

{ #category : #'public interface' }
BIConfigurableFormatter >> containerWidth: anInteger [
	self installNewValueInContext: {(#maxLineLength: -> anInteger)}
]

{ #category : #private }
BIConfigurableFormatter >> currentLineLength [
	^ codeStream position - lineStart
]

{ #category : #'public interface' }
BIConfigurableFormatter >> format: aParseTree [
	originalSource := aParseTree source.
	self visitNode: aParseTree.
	^ codeStream contents
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatArray: anArrayNode [
	| current | 
	(anArrayNode statements reject: [ :each | each className = 'RBLiteralValueNode' ])
		ifEmpty: [ 
			anArrayNode statements
				do: [ :each | 
					current := each. 
					self visitNode: each ]
				separatedBy: [  
					(self isLineTooLong: current value asString)
						ifTrue: [ 
							codeStream nextPut: $..
							self newLine ]
						ifFalse: [ codeStream nextPutAll: ' . ' ] ] ]
		ifNotEmpty: [ self formatSequenceNodeStatementsFor: anArrayNode ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatBlock: aBlockNode [
	codeStream nextPutAll: self stringInsideBlocks.
	self formatBlockArgumentsFor: aBlockNode.
	self formatBlockCommentFor: aBlockNode.
	self visitNode: aBlockNode body.
	(self lineUpBlockBrackets and: [ self willBeMultiline: aBlockNode body ])
		ifTrue: [ self newLine ]
		ifFalse: [ codeStream nextPutAll: self stringInsideBlocks ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatBlockArgumentsFor: aBlockNode [
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do:
			[ :each | codeStream nextPut: $:.
			self visitNode: each.
			self formatCommentWithStatements
				ifTrue: [ self formatCommentsFor: each ].
			self space ].
	codeStream nextPutAll: '| '.
	(self willBeMultiline: aBlockNode body)
		ifTrue: [ self newLine ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatBlockCommentFor: aBlockNode [
	originalSource isNil
		ifTrue: [  ^ self ].
	aBlockNode comments
		do: [ :each | 
			
			self basicFormatCommentFor: each.
			(self isNonEmptySingleLineBlock: aBlockNode)
				ifTrue: [  self space ].
			(self willBeMultiline: aBlockNode body)
				ifTrue: [  self newLine ] ]
]

{ #category : #accessing }
BIConfigurableFormatter >> formatCommentWithStatements [
	^ context formatCommentWithStatements
]

{ #category : #accessing }
BIConfigurableFormatter >> formatCommentWithStatements: aBoolean [
	context formatCommentWithStatements: aBoolean
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatCommentsFor: aNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aNode comments
		do:
			[ :each | 
			self basicFormatCommentFor: each.
			self newLine ]
]

{ #category : #private }
BIConfigurableFormatter >> formatMessageNodeInAMessageStreamFor: aMessageNode [
	| messageStream |
	messageStream := WriteStream on: (String new: 100).
	self
		with: aMessageNode selectorParts
		and: aMessageNode arguments
		do: [ :selector :arg | 
			messageStream
				nextPutAll: selector;
				space;
				nextPutAll: (self formattedSourceFor: arg) ]
		separatedBy: [ messageStream space ].
		^ messageStream 
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatMethodBodyFor: aMethodNode [
	self
		indentAround:
			[ 
			self newLines: self newLinesAfterMethodPattern.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatMethodCommentFor: aMethodNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	aMethodNode comments
		do:
			[ :each | 
			
			self useBasicCommentFormat 
			ifTrue: [ self basicFormatCommentFor: each ]
			ifFalse: [ 
			self resizeCommentFor: each startingAt: 0].
			self newLines: self newLinesAfterMethodComment ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatMethodPatternFor: aMethodNode [
	aMethodNode arguments isEmpty
		ifTrue: [ codeStream nextPutAll: aMethodNode selector ]
		ifFalse: [ 
			(self needsMethodSignatureOnMultipleLinesFor: aMethodNode)
				ifTrue: [ self privateFormatMethodPatternMultiLineFor: aMethodNode ]
				ifFalse: [ self privateFormatMethodPatternMonoLineFor: aMethodNode ] ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatPragmasFor: aMethodNode [
	aMethodNode pragmas do: [ :each | self visitNode: each; newLine ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatSelectorAndArguments: aMessageNode [
	| newLineBetweenArguments |
	newLineBetweenArguments := self isMultiLineMessage: aMessageNode.
	self
		indent:
			(newLineBetweenArguments
				ifTrue: [ self indentsForKeywords ]
				ifFalse: [ 0 ])
		around: [ 
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator:
					((newLineBetweenArguments or: [ self newLineBeforeFirstKeyword ])
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ])
				restSeparator:
					(newLineBetweenArguments
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ]) ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock [
	| separatorBlock |
	separatorBlock := firstBlock.
	aMessageNode isUnary
		ifTrue: [ 
			self handleLineForSelector: aMessageNode selector withSeparatorBlock:
				separatorBlock.
			codeStream nextPutAll: aMessageNode selector ]
		ifFalse: [ 
			aMessageNode selectorParts
				with: aMessageNode arguments
				do: [ :selector :argument | 
					self handleLineForSelector: selector withSeparatorBlock: separatorBlock.
					separatorBlock := restBlock.
					self
						indentAround: [ 
							codeStream nextPutAll: selector.
							self handleLineForArgument: argument ] ] ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatSequenceNodeStatementsFor: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty
		ifTrue: [ ^ self ].
	1 to: statements size do: [ :i | 
		self visitNode: (statements at: i).
		(i < statements size
			or: [ 
				aSequenceNode parent
					ifNil: [ self periodsAsTerminators ]
					ifNotNil: [ :parent | 
						parent isBlock
							ifTrue: [ self periodsAtEndOfBlock ]
							ifFalse: [ self periodsAtEndOfMethod ] ] ])
			ifTrue: [ codeStream nextPut: $. ].
		self formatStatementCommentsFor: (statements at: i).
		i < statements size
			ifTrue: [ self
					addNewLinesBeforeStatementStartingAt: (statements at: i + 1) start ] ]
]

{ #category : #'private-formatting' }
BIConfigurableFormatter >> formatStatementCommentsFor: aStatementNode [
	originalSource isNil
		ifTrue: [ ^ self ].
	self formatCommentWithStatements
		ifFalse: [ ^ self ].
	aStatementNode statementComments
		do:
			[ :each | 
			| count |
			count := self newLinesBeforeStartingAt: each start.
			self retainBlankLinesBeforeComments & (count > 0)
				ifTrue: [ self newLines: count ]
				ifFalse: [ codeStream tab ].
			self useBasicCommentFormat
				ifTrue: [ self basicFormatCommentFor: each ]
				ifFalse: [ 
			self resizeCommentFor: each startingAt: self currentLineLength ]]
]

{ #category : #private }
BIConfigurableFormatter >> formatTemporariesFor: aSequenceNode [
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
					self formatCommentWithStatements
						ifTrue: [ self formatStatementCommentsFor: each ].
					self space ] ].
	self newLines: self newLinesAfterTemporaries
]

{ #category : #private }
BIConfigurableFormatter >> formattedSourceFor: aNode [
	^ lookaheadCode
		at: aNode
		ifAbsentPut: [ 
			self class new
				indent: self indent;
				installNewContext: context;
				format: aNode ]
]

{ #category : #private }
BIConfigurableFormatter >> handleLineForArgument: anArgument [
	(self keepBlockInMessage and: [ anArgument isBlock ])
		ifTrue: [ (self willBeMultiline: anArgument)
				ifTrue: [ (self formattedSourceFor: anArgument) lines first size > self maxLineLength
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ]
				ifFalse: [ anArgument sourceInterval size > self maxLineLength
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ] ]
		ifFalse: [ ((self willBeMultiline: anArgument)
				or: [ anArgument sourceInterval size > self maxLineLength ])
				ifTrue: [ self isInCascadeNode
						ifTrue: [ codeStream nextPut: Character cr.
							self indent - 1 timesRepeat: [ codeStream nextPutAll: self indentString ] ]
						ifFalse: [ self newLine ] ]
				ifFalse: [ self space ] ].
	self visitNode: anArgument
]

{ #category : #private }
BIConfigurableFormatter >> handleLineForSelector: selector withSeparatorBlock: aBlock [
	(self isLineTooLong: selector)
		ifTrue: [ self newLine ]
		ifFalse: [ aBlock value ]
]

{ #category : #private }
BIConfigurableFormatter >> hasAMultiLineMessageArgument: anArgumentsCollection [
	^ anArgumentsCollection
		anySatisfy: [ :each | 
			self isInCascadeNode
				ifTrue: [ self indent: 0 around: [ self willBeMultiline: each ] ]
				ifFalse: [ 
					self
						indent: self indentsForKeywords + 1
						around: [ self willBeMultiline: each ] ] ]
]

{ #category : #accessing }
BIConfigurableFormatter >> indent [
	^ indent
]

{ #category : #accessing }
BIConfigurableFormatter >> indent: anInteger [

	indent := anInteger
]

{ #category : #private }
BIConfigurableFormatter >> indent: anInteger around: aBlock [ 
	self indent: self indent + anInteger.
	^aBlock ensure: [self indent: self indent - anInteger]
]

{ #category : #private }
BIConfigurableFormatter >> indentAround: aBlock [ 
	self indent: 1 around: aBlock
]

{ #category : #accessing }
BIConfigurableFormatter >> indentString [
	^ context indentString
]

{ #category : #accessing }
BIConfigurableFormatter >> indentString: aString [
	context indentString: aString
]

{ #category : #accessing }
BIConfigurableFormatter >> indentsForKeywords [
	^ context indentsForKeywords
]

{ #category : #accessing }
BIConfigurableFormatter >> indentsForKeywords: anInteger [
	context indentsForKeywords: anInteger
]

{ #category : #initialization }
BIConfigurableFormatter >> initialize [
	super initialize.
	lineStart := 0.
	self indent: 0.
	self isInCascadeNode: false.
	lookaheadCode := IdentityDictionary new.
	codeStream := WriteStream on: (String new: 256).
	context := DefaultPrettyPrintContext
]

{ #category : #'public interface' }
BIConfigurableFormatter >> installNewContext: aContext [

	context := aContext
]

{ #category : #'public interface' }
BIConfigurableFormatter >> installNewValueInContext: aCol [
	"self new installNewValueInContext: { (#cascadedMessageInsideParentheses: -> true) }"
	
	context := context copy.
	"needed because by default context point to the DefaultPrettyPrintContext"
	aCol do: [ :each | self perform: each key with: each value]
]

{ #category : #accessing }
BIConfigurableFormatter >> isInCascadeNode [
	^ isInCascadeNode
]

{ #category : #accessing }
BIConfigurableFormatter >> isInCascadeNode: aBoolean [
	isInCascadeNode := aBoolean
]

{ #category : #private }
BIConfigurableFormatter >> isLineTooLong: aString [
	^ self currentLineLength + (aString indexOf: Character cr ifAbsent: [ aString size ])
		>= self maxLineLength
]

{ #category : #private }
BIConfigurableFormatter >> isMultiLineMessage: aMessageNode [
	(self multiLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ true ].
	(self oneLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ false ].
	self numberOfArgumentsForMultiLine <= aMessageNode arguments size
		ifTrue: [ ^ true ].
	(self hasAMultiLineMessageArgument: aMessageNode arguments)
		ifTrue: [ ^ true ].
	aMessageNode isUnary
		ifTrue: [ ^ self isLineTooLong: aMessageNode selector ].
	^ self isLineTooLong: (self formatMessageNodeInAMessageStreamFor: aMessageNode) contents
]

{ #category : #private }
BIConfigurableFormatter >> isNonEmptySingleLineBlock: aBlockNode [
	^ ((self willBeMultiline: aBlockNode body) or: [ aBlockNode body statements isEmpty ]) not
]

{ #category : #accessing }
BIConfigurableFormatter >> keepBlockInMessage [
	^ context keepBlockInMessage
]

{ #category : #accessing }
BIConfigurableFormatter >> keepBlockInMessage: aBoolean [
	context keepBlockInMessage: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> lineStart [
	^ lineStart
]

{ #category : #accessing }
BIConfigurableFormatter >> lineStart: anObject [
	lineStart := anObject
]

{ #category : #accessing }
BIConfigurableFormatter >> lineUpBlockBrackets [
	^ context lineUpBlockBrackets
]

{ #category : #accessing }
BIConfigurableFormatter >> lineUpBlockBrackets: aBoolean [
	context lineUpBlockBrackets: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> maxLineLength [
	^ context maxLineLength
]

{ #category : #accessing }
BIConfigurableFormatter >> maxLineLength: anInteger [
	context maxLineLength: anInteger
]

{ #category : #accessing }
BIConfigurableFormatter >> methodSignatureOnMultipleLines [
	^ context methodSignatureOnMultipleLines
]

{ #category : #accessing }
BIConfigurableFormatter >> methodSignatureOnMultipleLines: aBoolean [
	context methodSignatureOnMultipleLines: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> minimumNewLinesBetweenStatements [
	^ context minimumNewLinesBetweenStatements
]

{ #category : #accessing }
BIConfigurableFormatter >> minimumNewLinesBetweenStatements: anInteger [
	context minimumNewLinesBetweenStatements: anInteger
]

{ #category : #accessing }
BIConfigurableFormatter >> multiLineMessages [
	^ context multiLineMessages
]

{ #category : #accessing }
BIConfigurableFormatter >> multiLineMessages: anArray [
	context multiLineMessages: anArray
]

{ #category : #private }
BIConfigurableFormatter >> needsMethodSignatureOnMultipleLinesFor: aMethodNode [
	| cpt |
	cpt := 0.
	self methodSignatureOnMultipleLines
		ifTrue:
			[ 
			aMethodNode selectorParts
				with: aMethodNode arguments
				do:
					[ :key :arg | 
					key size + arg name size > self selectorAndArgumentCombinedMaxSize
						ifTrue: [ cpt := cpt + 1 ] ].
			^ cpt > 1 ]
		ifFalse: [ ^ false ]
]

{ #category : #private }
BIConfigurableFormatter >> needsParenthesisFor: aNode [
	| parent grandparent |
	aNode ifNil: [ ^ false ].
	aNode isValue
		ifFalse: [ ^ false ].
	aNode isParseError ifTrue: [ ^false ].
	parent := aNode parent ifNil: [ ^ false ].
	(aNode isMessage and: [ 
		parent isMessage and: [ 
		parent receiver == aNode and:[
		aNode selector isUnary not ] ] ])
		ifTrue: [ 
			grandparent := parent parent.
			(grandparent notNil and: [ grandparent isCascade ])
				ifTrue: [ ^ true ] ].
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
	^ self useTraditionalBinaryPrecedenceForParentheses
		and: [ self precedenceOf: parent selector greaterThan: aNode selector ]
]

{ #category : #private }
BIConfigurableFormatter >> newLine [
	self newLines: 1
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineAfterCascade [
	^ context newLineAfterCascade
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineAfterCascade: aBoolean [
	context newLineAfterCascade: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineBeforeFirstCascade [
	^ context newLineBeforeFirstCascade
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineBeforeFirstCascade: aBoolean [
	context newLineBeforeFirstCascade: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineBeforeFirstKeyword [
	^ context newLineBeforeFirstKeyword
]

{ #category : #accessing }
BIConfigurableFormatter >> newLineBeforeFirstKeyword: aBoolean [
	context newLineBeforeFirstKeyword: aBoolean
]

{ #category : #private }
BIConfigurableFormatter >> newLines: anInteger [
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

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterMethodComment [
	^ context newLinesAfterMethodComment
]

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterMethodComment: anInteger [
	context newLinesAfterMethodComment: anInteger
]

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterMethodPattern [
	^ context newLinesAfterMethodPattern
]

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterMethodPattern: anInteger [
	context newLinesAfterMethodPattern: anInteger
]

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterTemporaries [
	^ context newLinesAfterTemporaries
]

{ #category : #accessing }
BIConfigurableFormatter >> newLinesAfterTemporaries: anInteger [
	context newLinesAfterTemporaries: anInteger
]

{ #category : #private }
BIConfigurableFormatter >> newLinesBeforeStartingAt: anIndex [
	| count cr lf index char |
	originalSource ifNil: [ ^ 0 ].
	(anIndex isNil or: [ anIndex > originalSource size ])
		ifTrue: [ ^ 0 ].
	cr := Character value: 13.
	lf := Character value: 10.
	count := 0.
	index := anIndex - 1.
	[ index > 0 and: [ (char := originalSource at: index) isSeparator ] ]
		whileTrue: [ 
			char == lf
				ifTrue: [ 
					count := count + 1.
					(originalSource at: (index - 1 max: 1)) == cr
						ifTrue: [ index := index - 1 ] ].
			char == cr
				ifTrue: [ count := count + 1 ].
			index := index - 1 ].
	^ count
]

{ #category : #accessing }
BIConfigurableFormatter >> numberOfArgumentsForMultiLine [
	^ context numberOfArgumentsForMultiLine
]

{ #category : #accessing }
BIConfigurableFormatter >> numberOfArgumentsForMultiLine: anInteger [
	context numberOfArgumentsForMultiLine: anInteger
]

{ #category : #accessing }
BIConfigurableFormatter >> oneLineMessages [
	^ context oneLineMessages
]

{ #category : #accessing }
BIConfigurableFormatter >> oneLineMessages: anArray [
	context oneLineMessages: anArray
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAsTerminators [
	^ context periodsAtEndOfBlock and: [ context periodsAtEndOfMethod ]
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAsTerminators: aBoolean [
	context periodsAtEndOfBlock: aBoolean.
	context periodsAtEndOfMethod: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAtEndOfBlock [
	^ context periodsAtEndOfBlock
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAtEndOfBlock: aBoolean [
	context periodsAtEndOfBlock: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAtEndOfMethod [
	^ context periodsAtEndOfMethod
]

{ #category : #accessing }
BIConfigurableFormatter >> periodsAtEndOfMethod: aBoolean [
	context periodsAtEndOfMethod: aBoolean
]

{ #category : #private }
BIConfigurableFormatter >> pragmaArgumentNeedsSeparator: anArgumentNode [
	^ anArgumentNode value isSymbol and: [ anArgumentNode value isBinary ]
]

{ #category : #private }
BIConfigurableFormatter >> precedenceOf: parentSelector greaterThan: childSelector [
	"Put parenthesis around things that are preceived to have 'lower' precedence. For example, 'a + b * c' 
	-> '(a + b) * c' but 'a * b + c' -> 'a * b + c'"

	| childIndex parentIndex |
	childIndex := 0.
	parentIndex := 0.
	1 to: self traditionalBinaryPrecedenceArray size do: [ :i | 
		((self traditionalBinaryPrecedenceArray at: i) includes: parentSelector first)
			ifTrue: [ parentIndex := i ].
		((self traditionalBinaryPrecedenceArray at: i) includes: childSelector first)
			ifTrue: [ childIndex := i ] ].
	^ childIndex < parentIndex
]

{ #category : #private }
BIConfigurableFormatter >> privateFormatMethodPatternMonoLineFor: aMethodNode [
	self
		with: aMethodNode selectorParts
		and: aMethodNode arguments
		do: [ :key :arg | 
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self characterSeparatorMethodSignatureFor: aMethodNode ]
]

{ #category : #private }
BIConfigurableFormatter >> privateFormatMethodPatternMultiLineFor: aMethodNode [
	| selectors arguments |
	arguments := aMethodNode arguments.
	selectors := aMethodNode selectorParts.
	codeStream nextPutAll: selectors first.
	self space.
	self visitNode: arguments first.
	codeStream nextPut: Character cr.
	self
		with: selectors allButFirst
		and: arguments allButFirst
		do: [ :key :arg | 
			codeStream nextPutAll: self indentString.
			codeStream nextPutAll: key.
			self space.
			self visitNode: arg ]
		separatedBy: [ self characterSeparatorMethodSignatureFor: aMethodNode].
	self newLines: 1
]

{ #category : #utilities }
BIConfigurableFormatter >> resizeComment: aComment withFirstLineShorterOf: anIndex [
	| cutComment firstLine currentCharPos |
	currentCharPos := 1.
	firstLine := (aComment contents withNoLineLongerThan: self maxLineLength - anIndex)
		lineNumber: 1.
	[ currentCharPos > firstLine size ]
		whileFalse:
			[ 
			codeStream nextPut: (firstLine at: currentCharPos).
			currentCharPos := currentCharPos + 1 ].
	currentCharPos > aComment contents size
		ifFalse:
			[ 
			self newLine.
			cutComment := aComment contents allButFirst: currentCharPos.
			self resizeStringDependingOnWindowSizeFor: cutComment ]
]

{ #category : #utilities }
BIConfigurableFormatter >> resizeCommentFor: aComment startingAt: anIndex [
	codeStream nextPut: $".
	"Hack to keep the right comment in pop up like cmd n, cmd m..."
	self maxLineLength < 50
		ifTrue: [ codeStream nextPutAll: aComment contents ]
		ifFalse:
			[ 
			aComment contents
				ifNotEmpty:
					[ 
					(anIndex = 0 or: [ anIndex >= self maxLineLength ])
						ifTrue: [ self resizeStringDependingOnWindowSizeFor: aComment ]
						ifFalse: [ self resizeComment: aComment withFirstLineShorterOf: anIndex ] ] ].
	codeStream nextPut: $"
]

{ #category : #utilities }
BIConfigurableFormatter >> resizeStringDependingOnWindowSizeFor: aComment [
	| resizedComment |
	resizedComment := aComment contents withNoLineLongerThan: self maxLineLength.
	1 to: resizedComment size do: [ :i | 
		codeStream nextPut: (resizedComment at: i).
		(resizedComment at: i) = Character cr
			ifTrue: [ self indent timesRepeat: [ codeStream nextPutAll: self indentString ] ] ]
]

{ #category : #accessing }
BIConfigurableFormatter >> retainBlankLinesBeforeComments [
	^ context retainBlankLinesBeforeComments
]

{ #category : #accessing }
BIConfigurableFormatter >> retainBlankLinesBeforeComments: aBoolean [
	context retainBlankLinesBeforeComments: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> retainBlankLinesBetweenStatements [
	^ context retainBlankLinesBetweenStatements
]

{ #category : #accessing }
BIConfigurableFormatter >> retainBlankLinesBetweenStatements: aBoolean [
	context retainBlankLinesBetweenStatements: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> selectorAndArgumentCombinedMaxSize [
	^ context selectorAndArgumentCombinedMaxSize
]

{ #category : #accessing }
BIConfigurableFormatter >> selectorAndArgumentCombinedMaxSize: anInteger [
	context selectorAndArgumentCombinedMaxSize: anInteger
]

{ #category : #private }
BIConfigurableFormatter >> space [
	codeStream space
]

{ #category : #accessing }
BIConfigurableFormatter >> stringFollowingReturn [
	^ context stringFollowingReturn
]

{ #category : #accessing }
BIConfigurableFormatter >> stringFollowingReturn: aString [
	context stringFollowingReturn: aString
]

{ #category : #accessing }
BIConfigurableFormatter >> stringInsideBlocks [
	^ context stringInsideBlocks
]

{ #category : #accessing }
BIConfigurableFormatter >> stringInsideBlocks: aString [
	context stringInsideBlocks: aString
]

{ #category : #accessing }
BIConfigurableFormatter >> stringInsideParentheses [
	^ context stringInsideParentheses
]

{ #category : #accessing }
BIConfigurableFormatter >> stringInsideParentheses: aString [
	context stringInsideParentheses: aString
]

{ #category : #accessing }
BIConfigurableFormatter >> traditionalBinaryPrecedenceArray [
	^ context traditionalBinaryPrecedenceArray
]

{ #category : #accessing }
BIConfigurableFormatter >> traditionalBinaryPrecedenceArray: anArray [
	context traditionalBinaryPrecedenceArray: anArray
]

{ #category : #accessing }
BIConfigurableFormatter >> useBasicCommentFormat [ 
	^ context useBasicCommentFormat 
]

{ #category : #accessing }
BIConfigurableFormatter >> useBasicCommentFormat: aBoolean [
	context useBasicCommentFormat: aBoolean
]

{ #category : #accessing }
BIConfigurableFormatter >> useTraditionalBinaryPrecedenceForParentheses [
	^ context useTraditionalBinaryPrecedenceForParentheses
]

{ #category : #accessing }
BIConfigurableFormatter >> useTraditionalBinaryPrecedenceForParentheses: aBoolean [
	context useTraditionalBinaryPrecedenceForParentheses: aBoolean
]

{ #category : #visiting }
BIConfigurableFormatter >> visitArrayNode: anArrayNode [
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitAssignmentNode: anAssignmentNode [ 
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self visitNode: anAssignmentNode value
]

{ #category : #visiting }
BIConfigurableFormatter >> visitBlockNode: aBlockNode [
	self
		bracketWith: '[]'
		around: [ self formatBlock: aBlockNode ].
	
]

{ #category : #visiting }
BIConfigurableFormatter >> visitCascadeNode: aCascadeNode [
	self isInCascadeNode: true.
	self visitNode: aCascadeNode receiver.
	self
		indentAround: [ 
			self newLineBeforeFirstCascade
				ifTrue: [ self newLine ]
				ifFalse: [ self space ].
			aCascadeNode messages
				do: [ :each | 
					self
						indentAround: [ 
							self formatCommentsFor: each.
							self
								formatSelectorAndArguments: each
								firstSeparator: [  ]
								restSeparator:
									((self isMultiLineMessage: each)
										ifTrue: [ [ self newLine ] ]
										ifFalse: [ [ self space ] ]) ] ]
				separatedBy: [ 
					codeStream nextPut: $;.
					self newLineAfterCascade
						ifTrue: [ self newLine ]
						ifFalse: [ self space ] ] ].
	self isInCascadeNode: false
]

{ #category : #visiting }
BIConfigurableFormatter >> visitLiteralArrayNode: aRBArrayLiteralNode [ 
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
BIConfigurableFormatter >> visitLiteralNode: aLiteralNode [
	aLiteralNode value isLiteral
		ifFalse: [ self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString: aLiteralNode sourceText ]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitLiteralVariableNode: aLiteralVariableNode [
	| binding |
	binding := aLiteralVariableNode binding.
	self writeString: binding key storeString, ' -> '.
	aLiteralVariableNode binding value isLiteral
		ifFalse: [ self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString:  binding value asString ]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitMessageNode: aMessageNode [ 
	self visitNode: aMessageNode receiver.
	self formatSelectorAndArguments: aMessageNode
]

{ #category : #visiting }
BIConfigurableFormatter >> visitMethodNode: aMethodNode [ 
	self formatMethodPatternFor: aMethodNode.
	self formatMethodBodyFor: aMethodNode
]

{ #category : #visiting }
BIConfigurableFormatter >> visitNode: aNode [
	| needsParenthesis |
	(lookaheadCode includesKey: aNode)
		ifTrue: [ ^ self writeString: (lookaheadCode at: aNode) ].
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: [ '()' ]
				ifFalse: [ '' ])
		around: [ 
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self stringInsideParentheses ].
			super visitNode: aNode.
			(self formatCommentWithStatements or: [ aNode isMethod or: [ aNode isSequence ] ])
				ifFalse: [ self formatCommentsFor: aNode ].
			needsParenthesis
				ifTrue: [ codeStream nextPutAll: self stringInsideParentheses ] ]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitParseErrorNode: aNode [
	self writeString: aNode value
]

{ #category : #visiting }
BIConfigurableFormatter >> visitPatternBlockNode: aRBPatternBlockNode [ 
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternBlockNode]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternWrapperBlockNode]
]

{ #category : #visiting }
BIConfigurableFormatter >> visitPragmaNode: aPragmaNode [
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
BIConfigurableFormatter >> visitReturnNode: aReturnNode [ 
	codeStream
		nextPut: $^;
		nextPutAll: self stringFollowingReturn.
	self visitNode: aReturnNode value
]

{ #category : #visiting }
BIConfigurableFormatter >> visitSequenceNode: aSequenceNode [
	self formatTemporariesFor: aSequenceNode.
	self formatCommentsFor: aSequenceNode.
	self formatSequenceNodeStatementsFor: aSequenceNode
]

{ #category : #visiting }
BIConfigurableFormatter >> visitVariableNode: aVariableNode [ 
	codeStream nextPutAll: aVariableNode name
]

{ #category : #private }
BIConfigurableFormatter >> willBeMultiline: aNode [ 
	^(self formattedSourceFor: aNode) includes: Character cr
]

{ #category : #utilities }
BIConfigurableFormatter >> with: firstCollection and: secondCollection do: aBlock separatedBy: separatorBlock [
	firstCollection isEmpty
		ifTrue: [ ^ self ].
	aBlock value: firstCollection first value: secondCollection first.
	2 to: firstCollection size do: [ :i | 
		separatorBlock value.
		aBlock value: (firstCollection at: i) value: (secondCollection at: i) ]
]

{ #category : #private }
BIConfigurableFormatter >> writeString: aString [
	| index |
	index := aString lastIndexOf: Character cr ifAbsent: [ 0 ].
	 codeStream nextPutAll: aString .
	index > 0
		ifTrue: [ lineStart := codeStream position - (aString size - index) ]
]
