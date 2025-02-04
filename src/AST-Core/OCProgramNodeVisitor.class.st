"
ASTProgramNodeVisitor is an abstract visitor for the ASTProgramNodes.


"
Class {
	#name : 'OCProgramNodeVisitor',
	#superclass : 'Object',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'testing' }
OCProgramNodeVisitor class >> isAbstract [

	^ self == OCProgramNodeVisitor
]

{ #category : 'API' }
OCProgramNodeVisitor >> visit: aMessageNode [
	^ aMessageNode acceptVisitor: self
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitAnnotationMarkNode: aAnnotationValueNode [
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"

	^ self visitVariableNode: anArgumentNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitArgumentNodes: aNodeCollection [
	"Sent *once* when visiting method and block nodes"

	^ aNodeCollection do: [ :each | self visitArgumentNode: each ]
]

{ #category : 'visiting - variables' }
OCProgramNodeVisitor >> visitArgumentVariableNode: anArgumentNode [

	^ self visitLocalVariableNode: anArgumentNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitArrayNode: anArrayNode [
	anArrayNode children do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitBlockNode: aBlockNode [
	self visitArgumentNodes: aBlockNode arguments.
	self visitNode: aBlockNode body
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitCascadeNode: aCascadeNode [
	aCascadeNode messages do: [ :each | self visitNode: each ]
]

{ #category : 'visiting - variables' }
OCProgramNodeVisitor >> visitClassVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitCommentNode: aComment [
	"Note: currenlty, comment nodes are not automatically visited by `ASTProgramNodeVisitor`.
	This method can still be reached by custom visitors and direct visit on a comment node."
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitEnglobingErrorNode: anEnglobingErrorNode [
	anEnglobingErrorNode contents do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitGlobalNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting - variables' }
OCProgramNodeVisitor >> visitGlobalVariableNode: aNode [

	^ self visitLiteralVariableNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitLiteralArrayNode: aLiteralArrayNode [
	aLiteralArrayNode contents do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitLiteralNode: aLiteralNode [
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitLiteralValueNode: aLiteralValueNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aLiteralValueNode
]

{ #category : 'visiting - variables' }
OCProgramNodeVisitor >> visitLiteralVariableNode: aNode [
	"to be backward compatible, we visit for Gloabls here (there used to be no difference)"

	^ self visitGlobalNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitLocalVariableNode: aNode [
	"call visitTemporaryNode: for backward compatibility"
	^ self visitTemporaryNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitMessageNode: aMessageNode [
	(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ])
		ifTrue: [ self visitNode: aMessageNode receiver ].
	self visitNode: aMessageNode selectorNode.
	aMessageNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitMethodNode: aMethodNode [
	self visitArgumentNodes: aMethodNode arguments.
	self visitPragmaNodes: aMethodNode pragmas.
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitNode: aNode [
	^ aNode acceptVisitor: self
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitParseErrorNode: anErrorNode [
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitPatternBlockNode: aPatternBlockNode [
	self visitArgumentNodes: aPatternBlockNode arguments.
	self visitNode: aPatternBlockNode body
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitPatternWrapperBlockNode: aPatternWrapperBlockNode [
	self visitNode: aPatternWrapperBlockNode wrappedNode.
	self visitArgumentNodes: aPatternWrapperBlockNode arguments.
	self visitNode: aPatternWrapperBlockNode body
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitPragmaNodes: aNodeCollection [
	"Sent *once* when visiting a method node"
	^ aNodeCollection do: [ :each | self visitPragmaNode: each ].
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitReturnNode: aReturnNode [
	^ self visitNode: aReturnNode value
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitSelectorNode: aSelectorNode [
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitSelfNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitSequenceNode: aSequenceNode [
	self visitTemporaryNodes: aSequenceNode temporaries.
	aSequenceNode statements do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [
	"| temp |
	temp is a temporary node as we can find in the body of methods, but it can't be visited the same way.
	Redirect the message on argumentNodeVisitor as a way to keep retrocompatibility"

	^ self visitArgumentNode: aTemporaryDeclarationNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitTemporaryNode: aNode [
	"Sent *each time* a temporary node is found"

	^ self visitVariableNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitTemporaryNodes: aNodeCollection [
	"This is triggered when defining the temporaries between the pipes"

	^ aNodeCollection
		do: [ :each | self visitTemporaryDeclarationNode: each ]
]

{ #category : 'visiting - variables' }
OCProgramNodeVisitor >> visitTemporaryVariableNode: aNode [

	^ self visitLocalVariableNode: aNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitThisContextNode: aThisContextNode [
	^ self visitVariableNode: aThisContextNode
]

{ #category : 'visiting' }
OCProgramNodeVisitor >> visitVariableNode: aVariableNode [
	^ aVariableNode
]
