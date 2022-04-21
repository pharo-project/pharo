"
A TASTProgramNodeVisitor is a simple Trait that define visitor methods. 
"
Trait {
	#name : #TASTProgramNodeVisitor,
	#category : #'AST-Core-Traits'
}

{ #category : #visiting }
TASTProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [ 
	"Sent *once* when visiting method and block nodes"
	^aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitArgumentVariableNode: anArgumentNode [

	^ self visitLocalVariableNode: anArgumentNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitArrayNode: anArrayNode [ 

	anArrayNode children do: [:each | self visitNode: each]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [

	self 
		visitNode: anAssignmentNode variable;
		visitNode: anAssignmentNode value
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitBlockNode: aBlockNode [ 
	self 
		visitArgumentNodes: aBlockNode arguments;
		visitNode: aBlockNode body
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitCascadeNode: aCascadeNode [ 
	aCascadeNode messages do: [:aMessage | self visitNode: aMessage]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitClassVariableNode: aNode [
	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitEnglobingErrorNode: anEnglobingErrorNode [
	anEnglobingErrorNode content do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitGlobalVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitLiteralValueNode: aNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitLiteralVariableNode: aNode [
	"to be backward compatible, we visit for Gloabls here (there used to be no difference)"

	^ self visitGlobalNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitLocalVariableNode: aNode [
	"call visitTemporaryNode: for backward compatibility"
	^ self visitTemporaryNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitMessageNode: aMessageNode [ 
	(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded]) 
		ifTrue: [self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | self visitNode: each]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitMethodNode: aMethodNode [ 
	self visitArgumentNodes: aMethodNode arguments.
	aMethodNode pragmas
		do: [:each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [ 
	self 
		visitArgumentNodes: aRBPatternBlockNode arguments;
		visitNode: aRBPatternBlockNode body
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [ 
	self 
		visitNode: aRBPatternWrapperBlockNode wrappedNode;
		visitArgumentNodes: aRBPatternWrapperBlockNode arguments;
		visitNode: aRBPatternWrapperBlockNode body
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitReturnNode: aReturnNode [ 
	^ self visitNode: aReturnNode value
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitSequenceNode: aSequenceNode [ 
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]
]

{ #category : #'visiting - reflectivity' }
TASTProgramNodeVisitor >> visitStoreIntoTempNode: aNode [
	^ aNode
]

{ #category : #'visiting - reflectivity' }
TASTProgramNodeVisitor >> visitStorePopIntoTempNode: aNode [
	^ aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	IT redirects the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitTemporaryVariableNode: aNode [
	
	^ self visitLocalVariableNode: aNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitUnreachableStatement: anUnreachableStatement [
	
	^ self visitEnglobingErrorNode: anUnreachableStatement
]

{ #category : #visiting }
TASTProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
