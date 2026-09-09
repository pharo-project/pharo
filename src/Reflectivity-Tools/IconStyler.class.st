"
I'm in charge to styling an ast with highlighting. To highglight something I need the rubric text model in textModel.

"
Class {
	#name : 'IconStyler',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'textModel',
		'stylerClasses',
		'renderer'
	],
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
IconStyler class >> isStaticStyler [

	^ false
]

{ #category : 'style' }
IconStyler class >> withStaticStylers [
	"Configure the receiver to invoke several static stylers. Now it is probably better to use the instance API:

	IconStyler new
		stylerClasses: { YourLovelyStyler };
		styleText: aTextModel withAST: anAST
	"

	^ self new
		  stylerClasses: (self allSubclasses select: #isStaticStyler);
		  yourself
]

{ #category : 'private' }
IconStyler >> addIconMethodStyle: aMethodNode [

	self addIconStyle: aMethodNode from: 1 to: 1
]

{ #category : 'private' }
IconStyler >> addIconStyle: aNode [

	self addIconStyle: aNode from: aNode start to: aNode stop + 1
]

{ #category : 'styling' }
IconStyler >> addIconStyle: aNode from: start to: stop [

	self addIconStyle: aNode from: start to: stop color: self highlightColor
]

{ #category : 'private' }
IconStyler >> addIconStyle: aNode from: start to: stop color: aColor [

	(self shouldStyleNode: aNode) ifFalse: [ ^ self ].
	renderer addDecoration: (IconStylerDecoration new
			 name: self stylerName;
			 interval: (start to: stop);
			 highlightColor: aColor;
			 icon: (self iconFor: aNode);
			 iconBlock: (self iconBlock: aNode);
			 title: (self iconLabel: aNode);
			 yourself)
]

{ #category : 'defaults' }
IconStyler >> highlightColor [
	^(Color green alpha: 0.1)
]

{ #category : 'defaults' }
IconStyler >> iconBlock: aNode [
	^ [ ]
]

{ #category : 'defaults' }
IconStyler >> iconFor: aNode [
	^nil
]

{ #category : 'defaults' }
IconStyler >> iconLabel: aNode [
	^ ''
]

{ #category : 'defaults' }
IconStyler >> iconLabelBlock: aNode [
	^ [self iconLabel: aNode]
]

{ #category : 'initialization' }
IconStyler >> initialize [

	super initialize.
	self resetStylerClasses.
	renderer := RubricIconStylerRenderer new
]

{ #category : 'accessing' }
IconStyler >> renderer [

	^ renderer
]

{ #category : 'accessing' }
IconStyler >> renderer: anObject [

	renderer := anObject
]

{ #category : 'initialization' }
IconStyler >> resetStylerClasses [
	stylerClasses := OrderedCollection new
]

{ #category : 'testing' }
IconStyler >> shouldStyleNode: aNode [
	^false
]

{ #category : 'styling' }
IconStyler >> styleAst: ast [
	"Style the AST using the textmodel of the receiver. Ask all the registered stylers to act."

	renderer clearDecorations.
	stylerClasses do: [ :styleClass | ast acceptVisitor: (styleClass new renderer: renderer) ]
]

{ #category : 'styling' }
IconStyler >> styleText: aTextModel withAst: ast [
	"Style the AST using the text model passed as first argument. Ask all the registered stylers to act."

	renderer textModel: aTextModel.
	renderer clearDecorations.
	stylerClasses do: [ :styleClass | ast acceptVisitor: (styleClass new renderer: renderer) ]
]

{ #category : 'accessing' }
IconStyler >> stylerClasses [
	^ stylerClasses
]

{ #category : 'accessing' }
IconStyler >> stylerClasses: anObject [
	stylerClasses := anObject
]

{ #category : 'private' }
IconStyler >> stylerName [

	^ 'icon'
]

{ #category : 'accessing' }
IconStyler >> textModel [
	^ textModel
]

{ #category : 'accessing' }
IconStyler >> textModel: anObject [

	textModel := anObject.
	renderer textModel: anObject
]

{ #category : 'accessing' }
IconStyler >> theme [
	^ Smalltalk ui theme
]

{ #category : 'visiting' }
IconStyler >> visitArgumentNode: anArgumentNode [
	self addIconStyle: anArgumentNode.
	super visitArgumentNode: anArgumentNode
]

{ #category : 'visiting' }
IconStyler >> visitArrayNode: anArrayNode [
	self addIconStyle: anArrayNode.
	super visitArrayNode: anArrayNode
]

{ #category : 'visiting' }
IconStyler >> visitAssignmentNode: anAssignmentNode [
	self addIconStyle: anAssignmentNode.
	super visitAssignmentNode: anAssignmentNode
]

{ #category : 'visiting' }
IconStyler >> visitBlockNode: aBlockNode [
	self addIconStyle: aBlockNode.
	super visitBlockNode: aBlockNode
]

{ #category : 'visiting' }
IconStyler >> visitCascadeNode: aCascadeNode [
	self addIconStyle: aCascadeNode.
	super visitCascadeNode: aCascadeNode
]

{ #category : 'visiting' }
IconStyler >> visitEnglobingErrorNode: anEnglobingNode [
	self addIconStyle: anEnglobingNode.
	anEnglobingNode contents do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
IconStyler >> visitLiteralArrayNode: aLiteralArrayNode [
	self addIconStyle: aLiteralArrayNode.
	super visitLiteralArrayNode: aLiteralArrayNode
]

{ #category : 'visiting' }
IconStyler >> visitLiteralNode: aLiteralNode [
	self addIconStyle: aLiteralNode.
	super visitLiteralNode: aLiteralNode
]

{ #category : 'visiting' }
IconStyler >> visitMessageNode: aMessageNode [
	self addIconStyle: aMessageNode.
	super visitMessageNode: aMessageNode
]

{ #category : 'visiting' }
IconStyler >> visitMethodNode: aMethodNode [
	self addIconMethodStyle: aMethodNode.
	super visitMethodNode: aMethodNode
]

{ #category : 'visiting' }
IconStyler >> visitParseErrorNode: anErrorNode [
	self addIconStyle: anErrorNode.
	super visitParseErrorNode: anErrorNode
]

{ #category : 'visiting' }
IconStyler >> visitPragmaNode: aPragmaNode [
	self addIconStyle: aPragmaNode.
	super visitPragmaNode: aPragmaNode
]

{ #category : 'visiting' }
IconStyler >> visitReturnNode: aReturnNode [
	self addIconStyle: aReturnNode.
	super visitReturnNode: aReturnNode
]

{ #category : 'visiting' }
IconStyler >> visitSequenceNode: aSequenceNode [
	self addIconStyle: aSequenceNode.
	super visitSequenceNode: aSequenceNode
]

{ #category : 'visiting' }
IconStyler >> visitVariableNode: aSuperNode [
	self addIconStyle: aSuperNode.
	super visitVariableNode: aSuperNode
]
