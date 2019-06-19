"
A TRBProgramNodeVisitor is a simple Trait that define visitor methods. 
"
Trait {
	#name : #TRBProgramNodeVisitor,
	#category : #'AST-Core-Traits'
}

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [ 
	"Sent *once* when visiting method and block nodes"
	^aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitArrayNode: anArrayNode [ 

	anArrayNode children do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [

	self 
		visitNode: anAssignmentNode variable;
		visitNode: anAssignmentNode value
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitBlockNode: aBlockNode [ 
	self 
		visitArgumentNodes: aBlockNode arguments;
		visitNode: aBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitCascadeNode: aCascadeNode [ 
	aCascadeNode messages do: [:aMessage | self visitNode: aMessage]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitMessageNode: aMessageNode [ 
	(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded]) 
		ifTrue: [self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitMethodNode: aMethodNode [ 
	self visitArgumentNodes: aMethodNode arguments.
	aMethodNode pragmas
		do: [:each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [ 
	self 
		visitArgumentNodes: aRBPatternBlockNode arguments;
		visitNode: aRBPatternBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [ 
	self 
		visitNode: aRBPatternWrapperBlockNode wrappedNode;
		visitArgumentNodes: aRBPatternWrapperBlockNode arguments;
		visitNode: aRBPatternWrapperBlockNode body
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitReturnNode: aReturnNode [ 
	^ self visitNode: aReturnNode value
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSequenceNode: aSequenceNode [ 
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	IT redirects the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : #visiting }
TRBProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
