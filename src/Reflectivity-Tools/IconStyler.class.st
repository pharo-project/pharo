"
I'm in charge to styling an ast with highlighting. To highglight something I need the rubric text model in textModel.

"
Class {
	#name : 'IconStyler',
	#superclass : 'RBProgramNodeVisitor',
	#instVars : [
		'textModel',
		'stylerClasses'
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

{ #category : 'styling' }
IconStyler >> addIconMethodStyle: aMethodNode [
	self addIconStyle: aMethodNode from: 1 to: 0
]

{ #category : 'styling' }
IconStyler >> addIconStyle: aNode [
	self addIconStyle: aNode from: aNode start to: aNode stop
]

{ #category : 'styling' }
IconStyler >> addIconStyle: aNode from: start to: stop [

	self addIconStyle: aNode from: start to: stop color: self highlightColor
]

{ #category : 'styling' }
IconStyler >> addIconStyle: aNode from: start to: stop color: aColor [

	| conf |
	(self shouldStyleNode: aNode) ifFalse: [ ^ self ].
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text |
		| r |
		r := self segmentMorphClass from: start to: stop + 1.
		text addSegment: r.
		r label: (self iconLabelBlock: aNode).
		r icon: (self iconFor: aNode).
		r iconBlock: (self iconBlock: aNode).
		r color: aColor.
		r borderColor: self borderColor ].
	textModel announce: conf
]

{ #category : 'defaults' }
IconStyler >> borderColor [
	^Color gray
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
	self resetStylerClasses
]

{ #category : 'initialization' }
IconStyler >> resetStylerClasses [
	stylerClasses := OrderedCollection new
]

{ #category : 'hooks' }
IconStyler >> segmentMorphClass [

	^ RubTextSegmentMorph
]

{ #category : 'testing' }
IconStyler >> shouldStyleNode: aNode [
	^false
]

{ #category : 'styling' }
IconStyler >> styleAst: ast [
	"Style the AST using the textmodel of the receiver. Ask all the registered stylers to act."

	stylerClasses do: [ :styleClass |
			ast acceptVisitor: (styleClass new textModel: textModel) ]
]

{ #category : 'styling' }
IconStyler >> styleText: aTextModel withAst: ast [
	"style the AST using the textmodel passed as first argument. Ask all the registered stylers to act."

	stylerClasses do: [ :styleClass |
		 ast acceptVisitor: (styleClass new textModel: aTextModel) ]
]

{ #category : 'accessing' }
IconStyler >> stylerClasses [
	^ stylerClasses
]

{ #category : 'accessing' }
IconStyler >> stylerClasses: anObject [
	stylerClasses := anObject
]

{ #category : 'accessing' }
IconStyler >> textModel [
	^ textModel
]

{ #category : 'accessing' }
IconStyler >> textModel: anObject [
	textModel := anObject
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
IconStyler >> visitLiteralArrayNode: aRBLiteralArrayNode [
	self addIconStyle: aRBLiteralArrayNode.
	super visitLiteralArrayNode: aRBLiteralArrayNode
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
