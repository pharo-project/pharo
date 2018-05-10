"
I'm the one who visits node to coloring the code
"
Class {
	#name : #SHRBTextStyler,
	#superclass : #SHTextStylerST80,
	#traits : 'TRBProgramNodeVisitor',
	#classTraits : 'TRBProgramNodeVisitor classTrait',
	#instVars : [
		'charAttr',
		'parentheseLevel',
		'bracketLevel'
	],
	#category : #'Shout-Styling'
}

{ #category : #'style table' }
SHRBTextStyler class >> initialTextAttributesForPixelHeight: aNumber [ 
	^ SHTextStylerST80 initialTextAttributesForPixelHeight: aNumber 
]

{ #category : #initialization }
SHRBTextStyler class >> initialize [
	"self initialize"
	styleTable := nil.
	textAttributesByPixelHeight := nil.
]

{ #category : #accessing }
SHRBTextStyler class >> styleTable [
	^ SHTextStylerST80 styleTable
]

{ #category : #accessing }
SHRBTextStyler class >> styleTable: anArray [ 
	SHTextStylerST80 styleTable: anArray 
]

{ #category : #accessing }
SHRBTextStyler class >> textAttributesByPixelHeight [  
	^ SHTextStylerST80 textAttributesByPixelHeight 
]

{ #category : #formatting }
SHRBTextStyler >> addAttributes: attributes forNode: anRBNode [
	self 
		addAttributes: attributes
		from: anRBNode start
		to: anRBNode stop
]

{ #category : #formatting }
SHRBTextStyler >> addAttributes: attributes from: start to: stop [
	charAttr
		from: start
		to: stop
		put: attributes
]

{ #category : #formatting }
SHRBTextStyler >> addStyle: styleSymbol attribute: additionalAttribute forNode: anRBNode [
	self 
		addStyle:styleSymbol
		attributes: { additionalAttribute }
		forNode: anRBNode.
]

{ #category : #formatting }
SHRBTextStyler >> addStyle: styleSymbol attribute: additionalAttribute from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol), {additionalAttribute} from: start to: end.
]

{ #category : #formatting }
SHRBTextStyler >> addStyle: styleSymbol attributes: additionalAttributes forNode: anRBNode [
	self 
		addAttributes: (self attributesFor: styleSymbol), additionalAttributes 
		forNode: anRBNode.
]

{ #category : #formatting }
SHRBTextStyler >> addStyle: styleSymbol forNode: anRBNode [
	self addAttributes: (self attributesFor: styleSymbol) forNode: anRBNode.
]

{ #category : #formatting }
SHRBTextStyler >> addStyle: styleSymbol from: start to: end [
	self addAttributes: (self attributesFor: styleSymbol) from: start to: end.
]

{ #category : #private }
SHRBTextStyler >> currentClosedBracketStyleName [
	bracketLevel isZero ifTrue: [ ^ #blockEnd ].
	^ (#blockEnd asString , bracketLevel asString) asSymbol
]

{ #category : #private }
SHRBTextStyler >> currentClosedParenthesisStyleName [
	parentheseLevel isZero ifTrue: [ ^ #rightParenthesis ].
	^ (#rightParenthesis asString , parentheseLevel asString) asSymbol
]

{ #category : #private }
SHRBTextStyler >> currentOpenedBracketStyleName [
	bracketLevel isZero ifTrue: [ ^ #blockStart ].
	^ (#blockStart asString , bracketLevel asString) asSymbol
]

{ #category : #private }
SHRBTextStyler >> currentOpenedParenthesisStyleName [
	parentheseLevel isZero ifTrue: [ ^ #leftParenthesis ].
	^ (#leftParenthesis asString , parentheseLevel asString) asSymbol
]

{ #category : #formatting }
SHRBTextStyler >> literalStyleSymbol: aValue [
	aValue isSymbol
		ifTrue: [ ^ #symbol ].
	aValue isString
		ifTrue: [ ^ #string ].
	aValue isCharacter
		ifTrue: [ ^ #character ].
	aValue isNumber
		ifTrue: [ ^ #number ].
	aValue == true
		ifTrue: [ ^ #true ].
	aValue == false
		ifTrue: [ ^ #false ].
	aValue isNil
		ifTrue: [ ^ #nil ].
	^ #default
]

{ #category : #private }
SHRBTextStyler >> methodOrBlockArgStyleFor: anArgumentNode [
	^ anArgumentNode isBlockVar
		ifTrue: [ #blockArg ]
		ifFalse: [ #methodArg ]
]

{ #category : #private }
SHRBTextStyler >> methodOrBlockTempDeclStyleFor: aSequenceNode [
	^ aSequenceNode scope isBlockScope
		ifTrue: [ #blockPatternTempVar ]
		ifFalse: [ #patternTempVar ]
]

{ #category : #private }
SHRBTextStyler >> methodOrBlockTempStyleFor: aTemporaryNode [
	^ aTemporaryNode isBlockVar
		ifTrue: [ #blockTempVar ]
		ifFalse: [ #tempVar ]
]

{ #category : #private }
SHRBTextStyler >> parse: aText isMethod: isMethod [
	|root|
	
	isMethod 
		ifTrue: [
			[root := RBParser parseFaultyMethod: aText asString.
			root methodNode methodClass: classOrMetaClass.
			^root] 
				on: Error 
				do: [^RBParser parseFaultyExpression: aText asString]]
		ifFalse:[ ^RBParser parseFaultyExpression: aText asString ].
]

{ #category : #private }
SHRBTextStyler >> privateStyle: aText [ 
    | ast |

    ast := self parse: aText isMethod: self isForWorkspace not.
    ast methodNode compilationContext: (CompilationContext new
            class: (classOrMetaClass ifNil: [UndefinedObject]);
				forSyntaxHighlighting: true;
            requestor: workspace).
    ast doSemanticAnalysis.
    self style: aText ast: ast.
    ^ aText
]

{ #category : #private }
SHRBTextStyler >> resolveStyleFor: aVariableNode [
	aVariableNode binding ifNil: [^#default].
	aVariableNode isArg ifTrue: [ ^#methodArg].
	aVariableNode isTemp ifTrue: [ ^#tempVar].
	aVariableNode isGlobal ifTrue: [ ^#globalVar].
	aVariableNode isInstance ifTrue: [ ^#instVar]. 
	aVariableNode hasIncompleteIdentifier ifTrue:[ ^#incompleteIdentifier].
	^#invalid.
]

{ #category : #private }
SHRBTextStyler >> resolveTextLinkFor: aVariableNode [

	aVariableNode binding isGlobalVariable
		ifTrue: [ ^ TextClassLink className: aVariableNode name ].
		
	^ TextVariableLink variableName: aVariableNode name
]

{ #category : #private }
SHRBTextStyler >> resolveVariableAttributesFor: aVariableNode [
	| textLink global |
	textLink := self resolveTextLinkFor: aVariableNode.
	aVariableNode binding isGlobalVariable ifFalse: [ ^{textLink}] .
	
	global :=	aVariableNode binding value.
	(global isClass and: [ global isDeprecated ]) ifFalse: [ ^{textLink} ].
	
	^{textLink. TextEmphasis struckOut}
]

{ #category : #private }
SHRBTextStyler >> style: aText ast: ast [
	text := aText.	
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	bracketLevel := 0.
	parentheseLevel:=0.
	ast acceptVisitor: self .
	aText runs: (RunArray newFrom: charAttr).	

]

{ #category : #private }
SHRBTextStyler >> styleCloseBracket: aBlockNode [
	bracketLevel := bracketLevel - 1.
	(aBlockNode right isZero or: [  aBlockNode value isNil ]) ifTrue:[^ self].
	self addStyle: self currentClosedBracketStyleName from: aBlockNode right to: aBlockNode right
]

{ #category : #private }
SHRBTextStyler >> styleCloseParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [ 
			aMessageNode parentheses
				reverseDo: [ :interval | 
					| pos |
					pos := interval last.
					parentheseLevel := parentheseLevel - 1.
					self addStyle: self currentClosedParenthesisStyleName from: pos to: pos ] ]
]

{ #category : #private }
SHRBTextStyler >> styleOpenBracket: aBlockNode [
	| style |
	style := aBlockNode isFaulty
		ifTrue: [ #invalid ]
		ifFalse: [ self currentOpenedBracketStyleName ].
	self addStyle: style from: aBlockNode left to: aBlockNode left.
	bracketLevel := bracketLevel + 1
]

{ #category : #private }
SHRBTextStyler >> styleOpenParenthese: aMessageNode [
	aMessageNode parentheses
		ifNotEmpty: [ 
			aMessageNode parentheses
				do: [ :interval | 
					| pos |
					pos := interval first.
					self addStyle:  self currentOpenedParenthesisStyleName from: pos to: pos.
					parentheseLevel := parentheseLevel + 1 ] ]
]

{ #category : #private }
SHRBTextStyler >> styleTempBars: aSequenceNode [
	| tempBarAttribute |
	tempBarAttribute := aSequenceNode scope isMethodScope
		ifTrue: [ #methodTempBar ]
		ifFalse: [ #blockTempBar ].
	aSequenceNode leftBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ].
	aSequenceNode rightBar
		ifNotNil: [ :pos | self addStyle: tempBarAttribute from: pos to: pos ]
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitArgumentNode: anArgumentNode [
	| blockOrMethodArgStyle |
	blockOrMethodArgStyle := self methodOrBlockArgStyleFor: anArgumentNode.
	self addStyle: blockOrMethodArgStyle forNode: anArgumentNode
]

{ #category : #visiting }
SHRBTextStyler >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value.
	anAssignmentNode variable binding isWritable
		ifFalse: [ self addStyle: #invalid forNode: anAssignmentNode ]
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitBlockNode: aBlockNode [
	aBlockNode comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	aBlockNode arguments do: [ :argument | self addStyle: #blockPatternArg forNode: argument ].
	aBlockNode bar ifNotNil: [ :pos | self addStyle: #blockArgsBar from: pos to: pos ].
	aBlockNode colons do: [ :pos | self addStyle: #blockArgColon from: pos to: pos ].
	self styleOpenBracket: aBlockNode.
	self visitNode: aBlockNode body.
	self styleCloseBracket: aBlockNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [ :each | self visitNode: each ].
	aCascadeNode semicolons do: [ :pos | self addStyle: #cascadeSeparator from: pos to: pos ]
]

{ #category : #visiting }
SHRBTextStyler >> visitLiteralArrayNode: aRBLiteralArrayNode [
	"in a (valid) byte array all elements are of the same type, style the whole contents
at once, but for ordinary literal arrays, style every node"

	(aRBLiteralArrayNode isForByteArray and: [ 
		aRBLiteralArrayNode isFaulty not and: [ 
			aRBLiteralArrayNode contents isNotEmpty 
		]
	]) 
	ifTrue: [ 
		self addStyle: #number 
			from: aRBLiteralArrayNode start + 2 
			to: aRBLiteralArrayNode stop - 1
	]
	ifFalse: [ aRBLiteralArrayNode contents do: [ :each | self visitNode: each ] ]
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitLiteralNode: aLiteralNode [
	| value |
	value := aLiteralNode value.
	self 
		addStyle: (self literalStyleSymbol: value)
		attribute: (TextClassLink class: value class)
		forNode: aLiteralNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitMessageNode: aMessageNode [
	| style link |
	style := #keyword.
	(Symbol findInternedSelector: aMessageNode selector asString)
		ifNil: [ 
			style := (Symbol selectorThatStartsCaseSensitive: aMessageNode selector asString skipping: nil) isNil
				ifTrue: [ #undefinedKeyword ]
				ifFalse: [ #incompleteKeyword ] ].
	link := TextMethodLink sourceNode: aMessageNode.
	self styleOpenParenthese: aMessageNode.
	aMessageNode selectorParts
		with: aMessageNode keywordsPositions
		do: [ :keyword :position | 
			self
				addStyle: style
				attribute: link
				from: position
				to: position + keyword size - 1 ].
	(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ])
		ifTrue: [ self visitNode: aMessageNode receiver ].
	aMessageNode arguments do: [ :each | self visitNode: each ].
	self styleCloseParenthese: aMessageNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitMethodNode: aMethodNode [
	| link |
	self visitMethodComments: aMethodNode comments.
	aMethodNode arguments do: [ :argument | self addStyle: #patternArg forNode: argument ].
	link := TextMethodLink selector: aMethodNode selector.
	aMethodNode selectorParts
		with: aMethodNode keywordsPositions
		do:
			[ :keyword :position | 
			self
				addStyle: #patternKeyword
				attribute: link
				from: position
				to: position + keyword size - 1 ].
	aMethodNode pragmas do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitNode: aNode [ 
	aNode comments do: [ :comment|
		self 
			addStyle: #comment
			from: comment start
			to: comment stop ].

	^aNode acceptVisitor: self
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitParseErrorNode: anErrorNode [
	self 
		addStyle: #invalid
		forNode: anErrorNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitPragmaNode: aPragmaNode [
	aPragmaNode selectorParts with: aPragmaNode keywordsPositions do: [ :keyword : position|
	self 
		addStyle: #pragmaKeyword
		from: position
		to: position + keyword size ].
	
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitReturnNode: aReturnNode [ 
	self 
		addStyle: #return
		from: aReturnNode start
		to: aReturnNode stop.
		
	self visitNode: aReturnNode value
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitSelfNode: aSelfNode [
	self 
		addStyle: #self
		forNode: aSelfNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitSequenceNode: aSequenceNode [
	| patternTempStyle |
	patternTempStyle := self methodOrBlockTempDeclStyleFor: aSequenceNode.
	self styleTempBars: aSequenceNode.
	aSequenceNode temporaries
		do: [ :temporary | self addStyle: patternTempStyle forNode: temporary ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode periods do: [ :pos | self addStyle: #statementSeparator from: pos to: pos ]
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitSuperNode: aSuperNode [
	self 
		addStyle: #super
		forNode: aSuperNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitTemporaryNode: aTemporaryNode [
	| methodOrBlockTempStyle |
	methodOrBlockTempStyle := self methodOrBlockTempStyleFor: aTemporaryNode.
	self addStyle: methodOrBlockTempStyle forNode: aTemporaryNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitThisContextNode: aThisContextNode [
	self 
		addStyle: #thisContext
		forNode: aThisContextNode
]

{ #category : #'visiting rb nodes' }
SHRBTextStyler >> visitVariableNode: aVariableNode [
	self 
		addStyle: (self resolveStyleFor: aVariableNode) 
		attributes: (self resolveVariableAttributesFor: aVariableNode)
		forNode: aVariableNode.
	
	
]
