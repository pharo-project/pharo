Class {
	#name : 'SHTextStyler',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'parentheseLevel',
		'braceLevel',
		'bracketLevel',
		'charAttr',
		'classOrMetaClass',
		'isScripting',
		'workspace',
		'view'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'formatting' }
SHTextStyler >> addAttributes: attributes forNode: aNode [

	self addAttributes: attributes from: aNode startWithoutParentheses to: aNode stopWithoutParentheses
]

{ #category : 'formatting' }
SHTextStyler >> addAttributes: attributes from: start to: stop [

	charAttr from: (start max: 1) to: (stop min: charAttr size) put: attributes
]

{ #category : 'converting' }
SHTextStyler >> addAttributesFrom: attributeRuns satisfying: aTestBlock to: aText [

	attributeRuns withStartStopAndValueDo: [:start :stop :attributes |
		attributes
			select: aTestBlock
			thenDo: [ :selectedAttribute|
				aText
					addAttribute: selectedAttribute
					from: start
					to: stop ]].

	^ aText
]

{ #category : 'formatting' }
SHTextStyler >> addStyle: styleSymbol attribute: additionalAttribute from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol), {additionalAttribute} from: start to: end
]

{ #category : 'formatting' }
SHTextStyler >> addStyle: styleSymbol attributes: additionalAttributes forNode: aNode [
	self
		addAttributes: (self attributesFor: styleSymbol), additionalAttributes
		forNode: aNode
]

{ #category : 'formatting' }
SHTextStyler >> addStyle: styleSymbol forNode: aNode [
	self addAttributes: (self attributesFor: styleSymbol) forNode: aNode
]

{ #category : 'formatting' }
SHTextStyler >> addStyle: styleSymbol from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol) from: start to: end
]

{ #category : 'private' }
SHTextStyler >> attributesFor: aSymbol [

	^ self class attributesFor: aSymbol
]

{ #category : 'accessing' }
SHTextStyler >> braceLevel [

	^ braceLevel
]

{ #category : 'accessing' }
SHTextStyler >> braceLevel: anObject [

	braceLevel := anObject
]

{ #category : 'accessing' }
SHTextStyler >> braceStyleName [

	| level |
	level := braceLevel \\ self maximumImbricationStyleSupported.
	^ ('brace' , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ]))
]

{ #category : 'accessing' }
SHTextStyler >> bracketLevel [

	^ bracketLevel
]

{ #category : 'accessing' }
SHTextStyler >> bracketLevel: anObject [

	bracketLevel := anObject
]

{ #category : 'accessing' }
SHTextStyler >> bracketStyleName [

	| level |
	level := bracketLevel \\ self maximumImbricationStyleSupported.
	^ ('block' , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ]))
]

{ #category : 'accessing' }
SHTextStyler >> charAttr [

	^ charAttr
]

{ #category : 'accessing' }
SHTextStyler >> charAttr: anObject [

	charAttr := anObject
]

{ #category : 'accessing' }
SHTextStyler >> classOrMetaClass [

	^ classOrMetaClass
]

{ #category : 'accessing' }
SHTextStyler >> classOrMetaClass: aBehavior [
	classOrMetaClass := aBehavior
]

{ #category : 'visiting' }
SHTextStyler >> formatIncompleteSelector: aSymbol [

	(self representsASelector: aSymbol) ifTrue: [ ^ nil ].

	^ (self class formatIncompleteIdentifiers and: [ self hasSymbolStartingBy: aSymbol ])
		  ifTrue: [ 'incompleteSelector' ]
		  ifFalse: [ 'undefinedSelector' ]
]

{ #category : 'testing' }
SHTextStyler >> hasSymbolStartingBy: aSymbol [

	^ (Symbol selectorThatStartsCaseSensitive: aSymbol asString skipping: nil) isNotNil
]

{ #category : 'initialization' }
SHTextStyler >> initialize [

	super initialize.
	isScripting := false
]

{ #category : 'accessing' }
SHTextStyler >> isScripting [
	^workspace
		ifNil: [ isScripting ]
		ifNotNil: [ workspace isScripting ]
]

{ #category : 'accessing' }
SHTextStyler >> isScripting: aBoolean [
	isScripting := aBoolean
]

{ #category : 'private' }
SHTextStyler >> literalStyleSymbol: aValue [

	aValue isSymbol
		ifTrue: [ ^ 'symbol' ].
	aValue isString
		ifTrue: [ ^ 'string' ].
	aValue isCharacter
		ifTrue: [ ^ 'character' ].
	aValue isNumber
		ifTrue: [ ^ 'number' ].
	aValue == true
		ifTrue: [ ^ 'true' ].
	aValue == false
		ifTrue: [ ^ 'false' ].
	aValue ifNil: [ ^ 'nil' ].
	^ 'default'
]

{ #category : 'accessing' }
SHTextStyler >> maximumImbricationStyleSupported [
	"We style up to 8 level of imbricated parenthesis/brackets/braces."

	^ 8
]

{ #category : 'accessing' }
SHTextStyler >> parentheseLevel [

	^ parentheseLevel
]

{ #category : 'accessing' }
SHTextStyler >> parentheseLevel: anObject [

	parentheseLevel := anObject
]

{ #category : 'accessing' }
SHTextStyler >> parenthesisStyleName [

	| level |
	level := parentheseLevel \\ self maximumImbricationStyleSupported.
	^ ('parenthesis' , (level isZero
		    ifTrue: [ '' ]
		    ifFalse: [ level asString ]))
]

{ #category : 'private' }
SHTextStyler >> privateStyle: aText [

	| compiler |
	aText ifEmpty: [ ^ self ].

	compiler := classOrMetaClass compiler
		            source: aText asString;
		            receiver: (self isScripting ifTrue: [ workspace ifNotNil: [ :w | w doItReceiver ] ]);
		            isScripting: self isScripting;
						bindings: (workspace ifNil: [#()] ifNotNil: [ workspace bindings]).

	^ self style: aText ast: compiler parse
]

{ #category : 'testing' }
SHTextStyler >> representsASelector: aSymbol [

	^ aSymbol isUsedAsSelectorSymbol
]

{ #category : 'private' }
SHTextStyler >> resolveStyleFor: aVariableNode [
	^ aVariableNode styleName
]

{ #category : 'styling' }
SHTextStyler >> style: aText [

	| text |
	aText ifEmpty: [ ^ self ].
	text := aText copy.
	self privateStyle: text.
	view ifNotNil: [ :view2 | view2 stylerStyled: text ]
]

{ #category : 'private' }
SHTextStyler >> styleCloseBrace: anArrayNode [

	braceLevel := braceLevel - 1.
	(anArrayNode right isZero or: [ anArrayNode value isNil ]) ifTrue: [ ^ self ].
	self addStyle: self braceStyleName from: anArrayNode right to: anArrayNode right
]

{ #category : 'private' }
SHTextStyler >> styleCloseBracket: aBlockNode [
	bracketLevel := bracketLevel - 1.
	(aBlockNode right isZero or: [ aBlockNode value isNil ]) ifTrue:[ ^self ].
	self addStyle: self bracketStyleName from: aBlockNode right to: aBlockNode right
]

{ #category : 'private' }
SHTextStyler >> styleCloseParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [
			aMessageNode parentheses
				do: [:interval |
					| pos |
					pos := interval last.
					parentheseLevel := parentheseLevel - 1.
					self addStyle: self parenthesisStyleName from: pos to: pos ]]
]

{ #category : 'private' }
SHTextStyler >> styleOpenBrace: anArrayNode [

	| style |
	style := self braceStyleName.
	self addStyle: style from: anArrayNode left to: anArrayNode left.
	braceLevel := braceLevel + 1
]

{ #category : 'private' }
SHTextStyler >> styleOpenBracket: aBlockNode [
	| style |
	style := self bracketStyleName.
	self addStyle: style from: aBlockNode left to: aBlockNode left.
	bracketLevel := bracketLevel + 1
]

{ #category : 'private' }
SHTextStyler >> styleOpenParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [
			aMessageNode parentheses
				reverseDo: [ :interval |
					| pos |
					pos := interval first.
					self addStyle:  self parenthesisStyleName from: pos to: pos.
					parentheseLevel := parentheseLevel + 1 ]]
]

{ #category : 'visiting' }
SHTextStyler >> styleParenthesisOf: aNode around: aBlock [

	aNode isValue ifFalse: [ ^ aBlock value ].

	self styleOpenParenthese: aNode.
	aBlock value.
	self styleCloseParenthese: aNode
]

{ #category : 'private' }
SHTextStyler >> styleTempBars: aSequenceNode [
	| tempBarAttribute |
	tempBarAttribute := (aSequenceNode scope isNil or: [ aSequenceNode scope isMethodScope ])
		ifTrue: [ 'methodTempBar' ]
		ifFalse: [ 'blockTempBar' ].
	aSequenceNode leftBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ].
	aSequenceNode rightBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ]
]

{ #category : 'styling' }
SHTextStyler >> styledTextFor: aText [

	^ self privateStyle: aText
]

{ #category : 'accessing' }
SHTextStyler >> view [

	^ view
]

{ #category : 'accessing' }
SHTextStyler >> view: aViewOrMorph [

	view := aViewOrMorph
]

{ #category : 'visiting' }
SHTextStyler >> visitAnnotationMarkNode: aAnnotationMarkNode [

	self addStyle: 'pragma' from: aAnnotationMarkNode start to: aAnnotationMarkNode start.
]

{ #category : 'visiting' }
SHTextStyler >> visitArrayNode: anArrayNode [

	anArrayNode comments do: [ :comment | self addStyle: 'comment' from: comment start to: comment stop ].
	self styleOpenBrace: anArrayNode.
	anArrayNode children do: [ :each | self visitNode: each ].
	self styleCloseBrace: anArrayNode
]

{ #category : 'visiting' }
SHTextStyler >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value.
	anAssignmentNode variable binding isWritable
		ifFalse: [ self addStyle: 'invalid' forNode: anAssignmentNode ]
]

{ #category : 'visiting' }
SHTextStyler >> visitBlockNode: aBlockNode [
	aBlockNode comments
		do:
			[ :comment | self addStyle: 'comment' from: comment start to: comment stop ].
	aBlockNode arguments do: [ :argument | self visitNode: argument ].
	aBlockNode bar
		ifNotNil: [ :pos | self addStyle: 'blockArgsBar' from: pos to: pos ].
	aBlockNode colons
		do: [ :pos | self addStyle: 'blockArgColon' from: pos to: pos ].
	self styleOpenBracket: aBlockNode.
	self visitNode: aBlockNode body.
	self styleCloseBracket: aBlockNode
]

{ #category : 'visiting' }
SHTextStyler >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [ :each | self visitNode: each ].
	aCascadeNode semicolons do: [ :pos | self addStyle: 'cascadeSeparator' from: pos to: pos ]
]

{ #category : 'visiting' }
SHTextStyler >> visitEnglobingErrorNode: anEnglobingErrorNode [

	anEnglobingErrorNode value ifNotEmpty: [
		self addStyle: 'invalid' from: anEnglobingErrorNode startWithoutParentheses to: anEnglobingErrorNode startWithoutParentheses + anEnglobingErrorNode value size - 1 ].
	anEnglobingErrorNode valueAfter ifNotEmpty: [
		self addStyle: 'invalid' from: anEnglobingErrorNode stopWithoutParentheses to: anEnglobingErrorNode stopWithoutParentheses + anEnglobingErrorNode valueAfter size - 1 ].
	anEnglobingErrorNode contents do: [:each | self visitNode: each ]
]

{ #category : 'visiting' }
SHTextStyler >> visitLiteralArrayNode: aLiteralArrayNode [
	"We reuse the nested brackets/parenthesis style.
	The main point it to easyly match opener and closer, nor necesarily to add type semanting"

	| style |
	aLiteralArrayNode isForByteArray
		ifTrue: [
			style := self bracketStyleName.
			bracketLevel := bracketLevel + 1 ]
		ifFalse: [
			style := self parenthesisStyleName.
			parentheseLevel := parentheseLevel + 1 ].

	self
		addStyle: style
		from: aLiteralArrayNode startWithoutParentheses
		to: aLiteralArrayNode startWithoutParentheses + aLiteralArrayNode openerSize - 1.
	self
		addStyle: style
		from: aLiteralArrayNode stopWithoutParentheses
		to: aLiteralArrayNode stopWithoutParentheses.

	aLiteralArrayNode contents do: [ :each | self visitNode: each ].

	aLiteralArrayNode isForByteArray
		ifTrue: [ bracketLevel := bracketLevel - 1 ]
		ifFalse: [ parentheseLevel := parentheseLevel - 1 ]
]

{ #category : 'visiting' }
SHTextStyler >> visitMethodNode: aMethodNode [
	| link |
	self visitMethodComments: aMethodNode.
	aMethodNode arguments do: [ :argument | self visitNode: argument ].
	link := TextMethodLink selector: aMethodNode selector.
	aMethodNode isDoIt ifFalse: [
		aMethodNode selectorParts
			with: aMethodNode keywordsPositions
			do:
				[ :keyword :position |
				self
					addStyle: 'patternSelector'
					attribute: link
					from: position
					to: position + keyword size - 1 ] ].
	aMethodNode pragmas do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
SHTextStyler >> visitNode: aNode [

	aNode comments do: [ :comment | self addStyle: 'comment' from: comment start to: comment stop ].

	^ self styleParenthesisOf: aNode around: [ aNode acceptVisitor: self ]
]

{ #category : 'visiting' }
SHTextStyler >> visitParseErrorNode: anErrorNode [

	self addStyle: 'invalid' forNode: anErrorNode
]

{ #category : 'visiting' }
SHTextStyler >> visitPragmaNode: aPragmaNode [
	"Lets imagin this pragma: `<  test: #toto test2: 2 >`

	We want to highlight the `<`, the `test:`, the `test2:` and the `>` but we do not want to highlight the arguments."

	self addStyle: 'pragma' from: aPragmaNode left to: aPragmaNode left.
	aPragmaNode keywordsIntervals do: [ :i | self addStyle: 'pragma' from: i first to: i last ].
	self addStyle: 'pragma' from: aPragmaNode right to: aPragmaNode right.

	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
SHTextStyler >> visitReturnNode: aReturnNode [

	self addStyle: 'return' from: aReturnNode start to: aReturnNode start.

	self visitNode: aReturnNode value
]

{ #category : 'visiting' }
SHTextStyler >> visitSequenceNode: aSequenceNode [

	self styleTempBars: aSequenceNode.
	aSequenceNode temporaries do: [ :each | self visitNode: each ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode periods do: [ :pos | self addStyle: 'statementSeparator' from: pos to: pos ]
]

{ #category : 'visiting' }
SHTextStyler >> visitVariableNode: aVariableNode [

	self addStyle: (self resolveStyleFor: aVariableNode) attributes: (self resolveVariableAttributesFor: aVariableNode) forNode: aVariableNode.
]

{ #category : 'accessing' }
SHTextStyler >> workspace [

	^ workspace
]

{ #category : 'accessing' }
SHTextStyler >> workspace: aWorkspace [
	workspace := aWorkspace
]
