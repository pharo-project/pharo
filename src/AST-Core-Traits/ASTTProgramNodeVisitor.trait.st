"
A TASTProgramNodeVisitor is a simple Trait that define visitor methods. 
"
Trait {
	#name : #ASTTProgramNodeVisitor,
	#category : #'AST-Core-Traits'
}

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [ 
	"Sent *once* when visiting method and block nodes"
	^aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitArgumentVariableNode: anArgumentNode [

	^ self visitLocalVariableNode: anArgumentNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitArrayNode: anArrayNode [ 

	anArrayNode children do: [:each | self visitNode: each]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [

	self 
		visitNode: anAssignmentNode variable;
		visitNode: anAssignmentNode value
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitBlockNode: aBlockNode [ 
	self 
		visitArgumentNodes: aBlockNode arguments;
		visitNode: aBlockNode body
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitCascadeNode: aCascadeNode [ 
	aCascadeNode messages do: [:aMessage | self visitNode: aMessage]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitClassVariableNode: aNode [
	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitEnglobingErrorNode: anEnglobingErrorNode [
	anEnglobingErrorNode content do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitGlobalVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitLiteralValueNode: aNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitLiteralVariableNode: aNode [
	"to be backward compatible, we visit for Gloabls here (there used to be no difference)"

	^ self visitGlobalNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitLocalVariableNode: aNode [
	"call visitTemporaryNode: for backward compatibility"
	^ self visitTemporaryNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitMessageNode: aMessageNode [ 
	(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded]) 
		ifTrue: [self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | self visitNode: each]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitMethodNode: aMethodNode [ 
	self visitArgumentNodes: aMethodNode arguments.
	aMethodNode pragmas
		do: [:each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [ 
	self 
		visitArgumentNodes: aRBPatternBlockNode arguments;
		visitNode: aRBPatternBlockNode body
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitPatternWrapperBlockNode: anASTPatternWrapperBlockNode [ 
	self 
		visitNode: anASTPatternWrapperBlockNode wrappedNode;
		visitArgumentNodes: anASTPatternWrapperBlockNode arguments;
		visitNode: anASTPatternWrapperBlockNode body
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitReturnNode: aReturnNode [ 
	^ self visitNode: aReturnNode value
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitSequenceNode: aSequenceNode [ 
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]
]

{ #category : #'visiting - reflectivity' }
ASTTProgramNodeVisitor >> visitStoreIntoTempNode: aNode [
	^ aNode
]

{ #category : #'visiting - reflectivity' }
ASTTProgramNodeVisitor >> visitStorePopIntoTempNode: aNode [
	^ aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	IT redirects the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitTemporaryVariableNode: aNode [
	
	^ self visitLocalVariableNode: aNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitUnreachableStatement: anUnreachableStatement [
	
	^ self visitEnglobingErrorNode: anUnreachableStatement
]

{ #category : #visiting }
ASTTProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
