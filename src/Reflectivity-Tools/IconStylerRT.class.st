"
I am a copy of the class IconStyler because I use my sub-classes to stylize the method tab and I just want to use a few select ones.
"
Class {
	#name : #IconStylerRT,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'textModel'
	],
	#category : #'Reflectivity-Tools-RealTimeStyling'
}

{ #category : #options }
IconStylerRT class >> shouldStyle [
	^true
]

{ #category : #style }
IconStylerRT class >> style: textModel [
	^self new textModel: textModel.
]

{ #category : #api }
IconStylerRT class >> styleText: textModel withAst: ast [
	self allSubclassesDo: [ :styleClass | 
			styleClass shouldStyle
				ifTrue: [ ast acceptVisitor: (styleClass style: textModel) ] ]
]

{ #category : #styling }
IconStylerRT >> addIconMethodStyle: aMethodNode [
	self addIconStyle: aMethodNode from: 1 to: 0
]

{ #category : #styling }
IconStylerRT >> addIconStyle: aNode [
	self addIconStyle: aNode from: aNode start to: aNode stop.
]

{ #category : #styling }
IconStylerRT >> addIconStyle: aNode from: start to: stop [
	
	self addIconStyle: aNode from: start to: stop color: self highlightColor 
]

{ #category : #styling }
IconStylerRT >> addIconStyle: aNode from: start to: stop color: aColor [
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
IconStylerRT >> borderColor [ 
	^Color gray
]

{ #category : #defaults }
IconStylerRT >> highlightColor [
	^(Color green alpha: 0.1)
]

{ #category : #defaults }
IconStylerRT >> iconBlock: aNode [
	^ [ ] 
]

{ #category : #defaults }
IconStylerRT >> iconFor: aNode [
	^nil
]

{ #category : #defaults }
IconStylerRT >> iconLabel: aNode [
	^ ''
]

{ #category : #defaults }
IconStylerRT >> iconLabelBlock: aNode [
	^ [self iconLabel: aNode]
]

{ #category : #testing }
IconStylerRT >> shouldStyleNode: aNode [
	^false 
]

{ #category : #accessing }
IconStylerRT >> textModel [
	^ textModel
]

{ #category : #accessing }
IconStylerRT >> textModel: anObject [
	textModel := anObject
]

{ #category : #visiting }
IconStylerRT >> visitArgumentNode: anArgumentNode [ 
	self addIconStyle: anArgumentNode.
	super visitArgumentNode: anArgumentNode.
]

{ #category : #visiting }
IconStylerRT >> visitArrayNode: anArrayNode [ 
	self addIconStyle: anArrayNode.
	super visitArrayNode: anArrayNode.
]

{ #category : #visiting }
IconStylerRT >> visitAssignmentNode: anAssignmentNode [ 
	self addIconStyle: anAssignmentNode.
	super visitAssignmentNode: anAssignmentNode.
]

{ #category : #visiting }
IconStylerRT >> visitBlockNode: aBlockNode [ 
	self addIconStyle: aBlockNode.
	super visitBlockNode: aBlockNode.
]

{ #category : #visiting }
IconStylerRT >> visitCascadeNode: aCascadeNode [ 
	self addIconStyle: aCascadeNode.
	super visitCascadeNode: aCascadeNode.
]

{ #category : #visiting }
IconStylerRT >> visitEnglobingErrorNode: anEnglobingNode [
	self addIconStyle: anEnglobingNode.
	anEnglobingNode content do: [ :each | self visitNode: each ].
]

{ #category : #visiting }
IconStylerRT >> visitGlobalNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitGlobalNode: aSelfNode.
]

{ #category : #visiting }
IconStylerRT >> visitInstanceVariableNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitInstanceVariableNode: aSelfNode.
]

{ #category : #visiting }
IconStylerRT >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	self addIconStyle: aRBLiteralArrayNode.
	super visitLiteralArrayNode: aRBLiteralArrayNode.
]

{ #category : #visiting }
IconStylerRT >> visitLiteralNode: aLiteralNode [ 
	self addIconStyle: aLiteralNode.
	super visitLiteralNode: aLiteralNode.
]

{ #category : #visiting }
IconStylerRT >> visitMessageNode: aMessageNode [ 
	self addIconStyle: aMessageNode.
	super visitMessageNode: aMessageNode.
]

{ #category : #visiting }
IconStylerRT >> visitMethodNode: aMethodNode [ 
	self addIconMethodStyle: aMethodNode.
	super visitMethodNode: aMethodNode.
]

{ #category : #visiting }
IconStylerRT >> visitParseErrorNode: anErrorNode [ 
	self addIconMethodStyle: anErrorNode.
	super visitParseErrorNode: anErrorNode.
]

{ #category : #visiting }
IconStylerRT >> visitPragmaNode: aPragmaNode [ 
	self addIconStyle: aPragmaNode.
	super visitPragmaNode: aPragmaNode.
]

{ #category : #visiting }
IconStylerRT >> visitReturnNode: aReturnNode [ 
	self addIconStyle: aReturnNode.
	super visitReturnNode: aReturnNode 
]

{ #category : #visiting }
IconStylerRT >> visitSelectorNode: aSelectorNode [
	self addIconStyle: aSelectorNode
]

{ #category : #visiting }
IconStylerRT >> visitSelfNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitSelfNode: aSelfNode 
]

{ #category : #visiting }
IconStylerRT >> visitSequenceNode: aSequenceNode [ 
	self addIconStyle: aSequenceNode.
	super visitSequenceNode: aSequenceNode.
]

{ #category : #visiting }
IconStylerRT >> visitSuperNode: aSuperNode [ 
	self addIconStyle: aSuperNode.
	super visitSuperNode: aSuperNode.
]

{ #category : #visiting }
IconStylerRT >> visitTemporaryNode: aNode [ 
	self addIconStyle: aNode.
	super visitTemporaryNode: aNode.
]

{ #category : #visiting }
IconStylerRT >> visitThisContextNode: aThisContextNode [ 
	self addIconStyle: aThisContextNode.
	super visitThisContextNode: aThisContextNode.
]

{ #category : #visiting }
IconStylerRT >> visitVariableNode: aNode [ 
	self addIconStyle: aNode.
	super visitVariableNode: aNode.
]
