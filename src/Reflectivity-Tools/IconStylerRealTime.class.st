"
I am a copy of the class IconStyler because I use my sub-classes to stylize the method tab and I just want to use a few select ones.
"
Class {
	#name : #IconStylerRealTime,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'textModel'
	],
	#category : #'Reflectivity-Tools-RealTimeStyling'
}

{ #category : #options }
IconStylerRealTime class >> shouldStyle [
	^true
]

{ #category : #style }
IconStylerRealTime class >> style: textModel [
	^self new textModel: textModel.
]

{ #category : #api }
IconStylerRealTime class >> styleText: textModel withAst: ast [
	self allSubclassesDo: [ :styleClass | 
			styleClass shouldStyle
				ifTrue: [ ast acceptVisitor: (styleClass style: textModel) ] ]
]

{ #category : #styling }
IconStylerRealTime >> addIconMethodStyle: aMethodNode [
	self addIconStyle: aMethodNode from: 1 to: 0
]

{ #category : #styling }
IconStylerRealTime >> addIconStyle: aNode [
	self addIconStyle: aNode from: aNode start to: aNode stop.
]

{ #category : #styling }
IconStylerRealTime >> addIconStyle: aNode from: start to: stop [
	
	self addIconStyle: aNode from: start to: stop color: self highlightColor 
]

{ #category : #styling }
IconStylerRealTime >> addIconStyle: aNode from: start to: stop color: aColor [
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
IconStylerRealTime >> borderColor [ 
	^Color gray
]

{ #category : #defaults }
IconStylerRealTime >> highlightColor [
	^(Color green alpha: 0.1)
]

{ #category : #defaults }
IconStylerRealTime >> iconBlock: aNode [
	^ [ ] 
]

{ #category : #defaults }
IconStylerRealTime >> iconFor: aNode [
	^nil
]

{ #category : #defaults }
IconStylerRealTime >> iconLabel: aNode [
	^ ''
]

{ #category : #defaults }
IconStylerRealTime >> iconLabelBlock: aNode [
	^ [self iconLabel: aNode]
]

{ #category : #testing }
IconStylerRealTime >> shouldStyleNode: aNode [
	^false 
]

{ #category : #accessing }
IconStylerRealTime >> textModel [
	^ textModel
]

{ #category : #accessing }
IconStylerRealTime >> textModel: anObject [
	textModel := anObject
]

{ #category : #visiting }
IconStylerRealTime >> visitArgumentNode: anArgumentNode [ 
	self addIconStyle: anArgumentNode.
	super visitArgumentNode: anArgumentNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitArrayNode: anArrayNode [ 
	self addIconStyle: anArrayNode.
	super visitArrayNode: anArrayNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitAssignmentNode: anAssignmentNode [ 
	self addIconStyle: anAssignmentNode.
	super visitAssignmentNode: anAssignmentNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitBlockNode: aBlockNode [ 
	self addIconStyle: aBlockNode.
	super visitBlockNode: aBlockNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitCascadeNode: aCascadeNode [ 
	self addIconStyle: aCascadeNode.
	super visitCascadeNode: aCascadeNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitEnglobingErrorNode: anEnglobingNode [
	self addIconStyle: anEnglobingNode.
	anEnglobingNode content do: [ :each | self visitNode: each ].
]

{ #category : #visiting }
IconStylerRealTime >> visitGlobalNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitGlobalNode: aSelfNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitInstanceVariableNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitInstanceVariableNode: aSelfNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	self addIconStyle: aRBLiteralArrayNode.
	super visitLiteralArrayNode: aRBLiteralArrayNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitLiteralNode: aLiteralNode [ 
	self addIconStyle: aLiteralNode.
	super visitLiteralNode: aLiteralNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitMessageNode: aMessageNode [ 
	self addIconStyle: aMessageNode.
	super visitMessageNode: aMessageNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitMethodNode: aMethodNode [ 
	self addIconMethodStyle: aMethodNode.
	super visitMethodNode: aMethodNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitParseErrorNode: anErrorNode [ 
	self addIconMethodStyle: anErrorNode.
	super visitParseErrorNode: anErrorNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitPragmaNode: aPragmaNode [ 
	self addIconStyle: aPragmaNode.
	super visitPragmaNode: aPragmaNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitReturnNode: aReturnNode [ 
	self addIconStyle: aReturnNode.
	super visitReturnNode: aReturnNode 
]

{ #category : #visiting }
IconStylerRealTime >> visitSelectorNode: aSelectorNode [
	self addIconStyle: aSelectorNode
]

{ #category : #visiting }
IconStylerRealTime >> visitSelfNode: aSelfNode [ 
	self addIconStyle: aSelfNode.
	super visitSelfNode: aSelfNode 
]

{ #category : #visiting }
IconStylerRealTime >> visitSequenceNode: aSequenceNode [ 
	self addIconStyle: aSequenceNode.
	super visitSequenceNode: aSequenceNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitSuperNode: aSuperNode [ 
	self addIconStyle: aSuperNode.
	super visitSuperNode: aSuperNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitTemporaryNode: aNode [ 
	self addIconStyle: aNode.
	super visitTemporaryNode: aNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitThisContextNode: aThisContextNode [ 
	self addIconStyle: aThisContextNode.
	super visitThisContextNode: aThisContextNode.
]

{ #category : #visiting }
IconStylerRealTime >> visitVariableNode: aNode [ 
	self addIconStyle: aNode.
	super visitVariableNode: aNode.
]
