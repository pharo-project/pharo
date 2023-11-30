"
RBProgramNodeVisitor is an abstract visitor for the RBProgramNodes.


"
Class {
	#name : 'ASTProgramNodeVisitor',
	#superclass : 'Object',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'class initialization' }
ASTProgramNodeVisitor class >> initialize [

	self deprecatedAliases: { #ASTProgramNodeVisitor }
]

{ #category : 'testing' }
ASTProgramNodeVisitor class >> isAbstract [

	^ self == ASTProgramNodeVisitor
]

{ #category : 'API' }
ASTProgramNodeVisitor >> visit: aRBMessageNode [
	^ aRBMessageNode acceptVisitor: self
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitAnnotationMarkNode: aRBAnnotationValueNode [
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [
	"Sent *once* when visiting method and block nodes"

	^ aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : 'visiting - variables' }
ASTProgramNodeVisitor >> visitArgumentVariableNode: anArgumentNode [

	^ self visitLocalVariableNode: anArgumentNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitArrayNode: anArrayNode [
	anArrayNode children do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitBlockNode: aBlockNode [
	self visitArgumentNodes: aBlockNode arguments.
	self visitNode: aBlockNode body
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [ :each | self visitNode: each ]
]

{ #category : 'visiting - variables' }
ASTProgramNodeVisitor >> visitClassVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitCommentNode: aRBComment [
	"Note: currenlty, comment nodes are not automatically visited by `RBProgramNodeVisitor`.
	This method can still be reached by custom visitors and direct visit on a comment node."
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitEnglobingErrorNode: anEnglobingErrorNode [
	anEnglobingErrorNode contents do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting - variables' }
ASTProgramNodeVisitor >> visitGlobalVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [
	aRBLiteralArrayNode contents do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitLiteralValueNode: aRBLiteralValueNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aRBLiteralValueNode
]

{ #category : 'visiting - variables' }
ASTProgramNodeVisitor >> visitLiteralVariableNode: aNode [
	"to be backward compatible, we visit for Gloabls here (there used to be no difference)"

	^ self visitGlobalNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitLocalVariableNode: aNode [
	"call visitTemporaryNode: for backward compatibility"
	^ self visitTemporaryNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitMessageNode: aMessageNode [
	(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ])
		ifTrue: [ self visitNode: aMessageNode receiver ].
	self visitNode: aMessageNode selectorNode.
	aMessageNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitMethodNode: aMethodNode [
	self visitArgumentNodes: aMethodNode arguments.
	self visitPragmaNodes: aMethodNode pragmas.
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitNode: aNode [
	^ aNode acceptVisitor: self
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitPatternBlockNode: aRBPatternBlockNode [
	self visitArgumentNodes: aRBPatternBlockNode arguments.
	self visitNode: aRBPatternBlockNode body
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	self visitArgumentNodes: aRBPatternWrapperBlockNode arguments.
	self visitNode: aRBPatternWrapperBlockNode body
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitPragmaNodes: aNodeCollection [
	"Sent *once* when visiting a method node"
	^ aNodeCollection do: [ :each | self visitPragmaNode: each ].
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitReturnNode: aReturnNode [
	^ self visitNode: aReturnNode value
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitSelectorNode: aSelectorNode [
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitSequenceNode: aSequenceNode [
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	Redirect the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitTemporaryNode: aNode [
	"Sent *each time* a temporary node is found"

	^ self visitVariableNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : 'visiting - variables' }
ASTProgramNodeVisitor >> visitTemporaryVariableNode: aNode [

	^ self visitLocalVariableNode: aNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : 'visiting' }
ASTProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
