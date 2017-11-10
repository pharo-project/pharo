"
I'm in charge to styling an ast with highlighting. To highglight something I need the rubric text model in textModel.

"
Class {
	#name : #IconStyler,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'textModel'
	],
	#category : #Reflectivity-Tools-Breakpoints
}

{ #category : #options }
IconStyler class >> shouldStyle [
	^true
]

{ #category : #style }
IconStyler class >> style: textModel [
	^self new textModel: textModel.
]

{ #category : #api }
IconStyler class >> styleText: textModel withAst: ast [
	self allSubclassesDo: [ :styleClass | 
			styleClass shouldStyle
				ifTrue: [ ast acceptVisitor: (styleClass style: textModel) ] ]
]

{ #category : #styling }
IconStyler >> addIconMethodStyle: aMethodNode [
	self addIconStyle: aMethodNode from: 1 to: 0
]

{ #category : #styling }
IconStyler >> addIconStyle: aNode [
	self addIconStyle: aNode from: aNode start to: aNode stop.
]

{ #category : #styling }
IconStyler >> addIconStyle: aNode from: start to: stop [
	
	self addIconStyle: aNode from: start to: stop color: self highlightColor 
]

{ #category : #styling }
IconStyler >> addIconStyle: aNode from: start to: stop color: aColor [
	|conf|
	(self shouldStyleNode: aNode) ifFalse: [ ^self ].
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text| |r| 
		r := RubTextSegmentMorph from: start to: stop + 1.
		text addSegment: r.
		
		r label: (self iconLabelBlock: aNode).
		r icon: (self iconFor: aNode).
		r iconBlock: (self iconBlock: aNode).
		r color: aColor.
		r borderColor: self borderColor.
	].

	textModel announce: conf
]

{ #category : #defaults }
IconStyler >> borderColor [ 
	^Color gray
]

{ #category : #defaults }
IconStyler >> highlightColor [
	^(Color green alpha: 0.1)
]

{ #category : #defaults }
IconStyler >> iconBlock: aNode [
	^ [ ] 
]

{ #category : #defaults }
IconStyler >> iconFor: aNode [
	^nil
]

{ #category : #defaults }
IconStyler >> iconLabel: aNode [
	^ ''
]

{ #category : #defaults }
IconStyler >> iconLabelBlock: aNode [
	^ [self iconLabel: aNode]
]

{ #category : #testing }
IconStyler >> shouldStyleNode: aNode [
	^false 
]

{ #category : #accessing }
IconStyler >> textModel [
	^ textModel
]

{ #category : #accessing }
IconStyler >> textModel: anObject [
	textModel := anObject
]

{ #category : #visiting }
IconStyler >> visitArgumentNode: anArgumentNode [ 
	self addIconStyle: anArgumentNode.
	super visitArgumentNode: anArgumentNode.
]

{ #category : #visiting }
IconStyler >> visitArrayNode: anArrayNode [ 
	self addIconStyle: anArrayNode.
	super visitArrayNode: anArrayNode.
]

{ #category : #visiting }
IconStyler >> visitAssignmentNode: anAssignmentNode [ 
	self addIconStyle: anAssignmentNode.
	super visitAssignmentNode: anAssignmentNode.
]

{ #category : #visiting }
IconStyler >> visitBlockNode: aBlockNode [ 
	self addIconStyle: aBlockNode.
	super visitBlockNode: aBlockNode.
]

{ #category : #visiting }
IconStyler >> visitCascadeNode: aCascadeNode [ 
	self addIconStyle: aCascadeNode.
	super visitCascadeNode: aCascadeNode.
]

{ #category : #visiting }
IconStyler >> visitGlobalNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitGlobalNode: aSelfNode.
]

{ #category : #visiting }
IconStyler >> visitInstanceVariableNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitInstanceVariableNode: aSelfNode.
]

{ #category : #visiting }
IconStyler >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	self addIconStyle: aRBLiteralArrayNode.
	super visitLiteralArrayNode: aRBLiteralArrayNode.
]

{ #category : #visiting }
IconStyler >> visitLiteralNode: aLiteralNode [ 
	self addIconStyle: aLiteralNode.
	super visitLiteralNode: aLiteralNode.
]

{ #category : #visiting }
IconStyler >> visitMessageNode: aMessageNode [ 
	self addIconStyle: aMessageNode.
	super visitMessageNode: aMessageNode.
]

{ #category : #visiting }
IconStyler >> visitMethodNode: aMethodNode [ 
	self addIconMethodStyle: aMethodNode.
	super visitMethodNode: aMethodNode.
]

{ #category : #visiting }
IconStyler >> visitParseErrorNode: anErrorNode [ 
	self addIconStyle: anErrorNode.
	super visitParseErrorNode: anErrorNode.
]

{ #category : #visiting }
IconStyler >> visitPragmaNode: aPragmaNode [ 
	self addIconStyle: aPragmaNode.
	super visitPragmaNode: aPragmaNode.
]

{ #category : #visiting }
IconStyler >> visitReturnNode: aReturnNode [ 
	self addIconStyle: aReturnNode.
	super visitReturnNode: aReturnNode 
]

{ #category : #visiting }
IconStyler >> visitSelfNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitSelfNode: aSelfNode 
]

{ #category : #visiting }
IconStyler >> visitSequenceNode: aSequenceNode [ 
	self addIconStyle: aSequenceNode.
	super visitSequenceNode: aSequenceNode.
]

{ #category : #visiting }
IconStyler >> visitSuperNode: aSuperNode [ 
	self addIconStyle: aSuperNode.
	super visitSuperNode: aSuperNode.
]

{ #category : #visiting }
IconStyler >> visitTemporaryNode: aNode [ 
	self addIconStyle: aNode.
	super visitTemporaryNode: aNode.
]

{ #category : #visiting }
IconStyler >> visitThisContextNode: aThisContextNode [ 
	self addIconStyle: aThisContextNode.
	super visitThisContextNode: aThisContextNode.
]
